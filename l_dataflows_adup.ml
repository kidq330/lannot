(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's Lannotate plug-in.                 *)
(*                                                                        *)
(*  Copyright (C) 2012-2022                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file LICENSE)                       *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype
open Ast_const

(** This file is used to generate dataflow criteria and is structured this way :
    The main AST visitor is addSequences :

    1- For each function, before visiting it, we perform a dataflow analysis
    (cf. dataflow analysis part).

    2- After the dataflow analysis, we visit the function, and add all
    annotations created by the dataflow analysis (Def / Use / Status variable)

    a) to_add_activators, to_add_comps, to_add_deactivators and to_add_fun are
    used to place created def/use from visit_defuse

    b) If the statement is a Def of V * find in def_to_predecessing_defs all
    preceding defs of V * for each def, find in def_to_deactivators all
    conditions for this def * Remember these conditions for this statement in
    to_add_deactivators Data flow analysis (do_function) :

    3- Initialize the initial state for the analysis (containing definitions
    from function's parameters)

    4- Perform the analysis by propagating a state (Defs set, Uses set) :

    a) Visit all expressions to find all uses in the function, and put them in
    the set Uses in our state. Join with intersection.

    b) Visit all definitions to find all defs in the function, and put them in
    the set Defs in our state. Join with union. When computing a Def of V,
    remove V from Uses, and remove V from Defs if CleanDataflow is true

    5- Then we iterate on all statements with their corresponding state, and
    visit (visit_defuse) this statement.

    a) For each use of V, we create a sequence for each Def of V is our state

    b) If this statement is a Def of V, we remember in def_to_predecessing_defs
    all defs of V that can reach this statements, which will be used in
    addSequences to create condition statements, i.e. reset of status variables
*)

module Graph = struct
  let rec detect_cycle ?(visited : Stmt.Set.t = Stmt.Set.empty) (v : Stmt.t) =
    if Stmt.Set.mem v visited then true
    else
      let vis_with_v = Stmt.Set.add v visited in
      let rec aux = function
        | [] -> false
        | stmt :: tl ->
            if detect_cycle ~visited:vis_with_v stmt then true else aux tl
      in
      aux v.succs
end

(** Used to represent a Lval with a truncated offset (trunc at first Index) *)
module VarOfst =
  Datatype.Pair_with_collections (Varinfo) (Offset)
    (struct
      let module_name = "L_dataflow_adup.VarOfst"
    end)

(** Used to represent a definition *)
module Def =
  Datatype.Triple_with_collections
    (Datatype.Int)
    (* Unique ID for each different definition of VarOfst *)
    (VarOfst)
    (* Store varinfo and truncated offset of this definition *)
    (Kinstr)
    (* KGlobal for function's parameters, KStmt otherwise *)
    (struct
      let module_name = "L_dataflow_adup.Def"
    end)

(** Used to represent a use *)
module Use =
  Datatype.Pair_with_collections
    (VarOfst)
    (* Store varinfo and truncated offset of this use  *)
    (Stmt)
    (* Use's statement *)
    (struct
      let module_name = "L_dataflow_adup.Use"
    end)

(** Module to add 2 tool functions *)
module Listtbl (T : Hashtbl.S) = struct
  include T

  (** Add a list in front of the existing binding, else create a new binding *)
  let add_list tbl key elt =
    match T.find_opt tbl key with
    | None -> T.add tbl key elt
    | Some old -> T.replace tbl key (elt @ old)

  (** Add an element in front of the existing binding, else bind to a new list
      containing this element *)
  let add tbl key elt = add_list tbl key [ elt ]
end

module Stmt_lst = Listtbl (Stmt.Hashtbl)
module Def_lst = Listtbl (Def.Hashtbl)

(** Represent states in our dataflow analysis *)
type t = Bottom | NonBottom of (Def.Set.t * Use.Set.t)

module Data = struct
  (** def_id: For each VarOfst, keep the number of assignment seen for this
      VarOfst *)
  (* key : VarOfst.t *)
  (* value : int *)

  (** seen_def: each def is registered to avoid creating them more than once *)
  (* key : stmt *)
  (* value : Def.t *)

  (** to_add_activators, to_add_comps, def_to_predecessing_defs: bind each new
      sequence to its corresponding statement, sequence id (int) is used to sort
      them at the end *)
  (* key : stmt *)
  (* value : (int * stmt) list *)

  (** def_to_deactivators: Bind to each def the list of conditions related to it
  *)
  (* key : Def.t *)
  (* value : (int * stmt) list *)

  type t = {
    def_id : int VarOfst.Hashtbl.t;
    seen_def : Def.t Stmt.Hashtbl.t;
    to_add_activators : (int * stmt) list Stmt.Hashtbl.t;
    to_add_comps : (int * stmt) list Stmt.Hashtbl.t;
    def_to_predecessing_defs : Def.Set.t Stmt.Hashtbl.t;
    def_to_deactivators : (int * stmt) list Def.Hashtbl.t;
    du_pairs : (Def.t * stmt) Seq.t ref;
  }

  let create () =
    {
      def_id = VarOfst.Hashtbl.create 32;
      seen_def = Stmt.Hashtbl.create 32;
      to_add_activators = Stmt.Hashtbl.create 32;
      to_add_comps = Stmt.Hashtbl.create 32;
      def_to_predecessing_defs = Stmt.Hashtbl.create 32;
      def_to_deactivators = Def.Hashtbl.create 32;
      du_pairs = ref Seq.empty;
    }

  (** Clear all hashtbls after each function in addSequences *)
  let reset_all (t : t) =
    VarOfst.Hashtbl.reset t.def_id;
    Stmt.Hashtbl.reset t.seen_def;
    Stmt.Hashtbl.reset t.to_add_activators;
    Stmt.Hashtbl.reset t.to_add_comps;
    Stmt.Hashtbl.reset t.def_to_predecessing_defs;
    Def.Hashtbl.reset t.def_to_deactivators;
    t.du_pairs := Seq.empty

  (** Given a VarOfst, increment the number of defs seen and returns it. If it
      is the first return 1. *)
  let get_next_def_id t varofst =
    match VarOfst.Hashtbl.find_opt t.def_id varofst with
    | None ->
        VarOfst.Hashtbl.add t.def_id varofst 1;
        1
    | Some n ->
        let new_val = n + 1 in
        VarOfst.Hashtbl.replace t.def_id varofst new_val;
        new_val

  let add_new_def t = Stmt.Hashtbl.add t.seen_def
  let lookup_seen_defs t = Stmt.Hashtbl.find_opt t.seen_def
  let add_def_deactivator t = Def_lst.add t.def_to_deactivators
  let add_comparison t = Stmt_lst.add t.to_add_comps
  let add_activator t = Stmt_lst.add t.to_add_activators
  let save_predecessing_defs t = Stmt.Hashtbl.add t.def_to_predecessing_defs

  let lookup_predecessing_defs t =
    Stmt.Hashtbl.find_opt t.def_to_predecessing_defs

  let lookup_def_deactivators t = Def.Hashtbl.find_opt t.def_to_deactivators

  let gather_activators t stmt =
    Stmt.Hashtbl.find_def t.to_add_activators stmt []

  let gather_comp_labels t stmt = Stmt.Hashtbl.find_def t.to_add_comps stmt []
end

let data = Data.create ()

(** Given an offset, truncate it before the first Index
    var->field1->filed2[42]->field3 will become var->field1->field2 *)
let rec trunc_fields offset =
  match offset with
  | Field (f_info, offset') ->
      let offset' = trunc_fields offset' in
      Field (f_info, offset')
  | Index _ | NoOffset -> NoOffset

(** For a given VarOfst V, statement S1 and set of Uses (from dataflow analysis
    state) Try to find a match (V', S2) in Uses such that V equals V' and S1
    post-dominate S2 *)
let is_equivalent varofst stmt kf uses =
  Use.Set.exists
    (fun (varofst', stmtUse) ->
      let eq = VarOfst.equal varofst' varofst in
      (* Our dataflow analysis garanty that uses in state always dominate the
         current statement S1 *)
      if eq && not (Dominators.dominates stmtUse stmt) then
        Options.fatal "Discrepancy: This should not happen";
      eq && Postdominators.is_postdominator kf ~opening:stmtUse ~closing:stmt)
    uses

(** Consider uses of V in an expression only once. In v2 = v1 + v1, we will
    consider 1 use of v1 *)
let is_triv_equiv varofst visited =
  Options.CleanEquiv.get () && List.exists (VarOfst.equal varofst) visited

let seq_status_prefix = "__SEQ_STATUS"
let seq_tmp_prefix = "__SEQ_TMP"

let mk_name ?(pre = seq_status_prefix) s id =
  pre ^ "_" ^ s ^ "_" ^ string_of_int id

(** Test if a VarOfst V should be instrumented, i.e. if it's not a global, a
    temp variable or if it's the first time we see it in the current expr/instr
    (except if CleanDuplicate is false) *)
let should_instrument ?(visited : VarOfst.t list option) varofst =
  let v, _offset = varofst in
  (not v.vglob)
  (* check if this is not a temporary variable in the sense that it is not some helper generated 
    by way of CIL normalization *)
  && (not v.vtemp)
  (* __retres is a special variable for the return value of the function *)
  && (not @@ Datatype.String.equal v.vname "__retres")
  (* check if this is not a variable already added by our instrumentation *)
  && (not @@ String.starts_with ~prefix:seq_status_prefix v.vname)
  (* LAnnotate global instrumentable var rules *)
  && Annotators.shouldInstrumentVar v
  && visited
     |> Option.map (fun visited -> not (is_triv_equiv varofst visited))
     |> Option.value ~default:true

let unk_loc = Location.unknown

(***********************************)
(********* Defuse Criteria *********)
(***********************************)

(** Perform the part 5- of the heading comment *)
class visit_defuse
  (* the dataflow analysis state for the current statement, consisting of definitions and uses found _up to this point_ *)
    (defs_set, uses_set)
  (* the statement for this visitor, it is important to note that all instances of this visitors have a single corresponding statement *)
  (* __jm__ use of this variable also suggests that the visited statement will necessarily be a use *)
    current_stmt
  (* the overarching kernel function, for which a dataflow analysis was performed *)
    kf
    (* a function specifying how to create instrumenting lines, should be a dependency injection coming from outside this module *)
  =
  object (_)
    inherit Visitor.frama_c_inplace

    (* invariant: no duplicate entries *)
    val mutable visited = []
    (** Used to avoid trivially equivalent uses *)

    val mutable visited_stmt = false
    (** should be treated as a debug variable for asserting that only one
        statement is visited throughout the lifetime of this visitor object *)

    method private init_vinfo ?(typ = Cil.intType) name =
      Cil.makeVarinfo false false name typ
    (** Create a new varinfo *)

    method private mk_set ?(loc = unk_loc) vi value =
      Ast_info.mkassign (Var vi, NoOffset) value loc |> Stmt_builder.instr
    (** Create a statement : vi = value; *)

    method private mk_comp ?(loc = unk_loc) ?(op = Cil_types.Eq)
        ?(offset = NoOffset) vi value =
      let new_exp = Exp_builder.mk ~loc (Lval (Var vi, offset)) in
      Exp_builder.binop op new_exp value
    (** Create a expression : Lval(vi,offset) == value *)

    method private mk_init ?(loc = unk_loc) vi value =
      Local_init (vi, AssignInit (SingleInit value), loc) |> Stmt_builder.instr
    (** Create a statement : typ vi = value; where typ is the type of vi *)

    (* __jm__ IMPORTANT: a single instance of visit_defuse is meant to visit a single statement - this is enforced with the assert at the beginning *)
    (* to oversimplify - this overload is meant to find `defs` *)
    method! vstmt_aux stmt =
      assert (Stmt.equal stmt current_stmt);
      assert (not visited_stmt);
      visited_stmt <- true;

      match stmt.skind with
      (* NOTE: is_label does not check whether the statement is a control flow label as used by goto constructs,
         but rather pc_label instrumentation instructions. This might be factored out if we ensure the instrumentation
         phase is done in its entirety AFTER the analysis *)
      | Instr i when not (Utils.is_label i) -> (
          match i with
          (* if matches this branch, this is a _def_ *)
          (* __jm__ jab at the Cil model, but how is a function call returning a result not also a Set instruction from the point of view of the AST? *)
          | Set ((Var v, offset), _, _) | Call (Some (Var v, offset), _, _, _)
            ->
              let varofst = (v, trunc_fields offset) in
              (if should_instrument varofst then
                 (* gathers all defs referencing the same variable as this particular def we are visiting, 
                   and notes that deactivators must be put before it *)
                 let v_defs =
                   Def.Set.filter
                     (fun (_, varData, _) -> VarOfst.equal varData varofst)
                     defs_set
                 in
                 if not @@ Def.Set.is_empty v_defs then
                   Data.save_predecessing_defs data stmt v_defs);
              Cil.DoChildren
          | _ -> Cil.DoChildren)
      | _ -> assert false
    (** Part 5- b) of the heading comment *)

    (* visits an AST expression node and it's children recursively. If the expression is an lvalue declaration,
       adds it to the list `visited`, as a way of tracking multiple uses of the same variable.
       Meant primarily to detect `uses` of an lvalue. *)
    (* to oversimplify - this overload is meant to identify `uses` *)
    (* __jm__ I have doubts about the efficiency of this approach, as it looks to be top-down first, for each
       first appearance of an lval, it scans defs_set and visited, as opposed to first building up `visited` as
       a unique set of lvals in the expression, and then performing the bookkeeping. *)
    method! vexpr expr =
      let pretty_def fmt (defId, (vi, ofst), stmt) =
        let stmt =
          match stmt with
          | Kglobal -> "Function parameter"
          | Kstmt stmt -> Format.asprintf "Stmt %a" Printer.pp_stmt stmt
        in
        Format.fprintf fmt "defId: %d / var : %a / %s@." defId
          Cil_printer.pp_lval (Var vi, ofst) stmt
      in

      let pretty_use fmt (((vi, ofst), stmt) : Use.t) =
        Format.fprintf fmt "var: %a / Stmt: %a@." Printer.pp_lval (Var vi, ofst)
          Printer.pp_stmt stmt
      in

      match expr.enode with
      (* entering this branch basically means we found a use(v) *)
      | Lval (Var v, offset) ->
          let varofst = (v, trunc_fields offset) in
          (* should_instrument in particular checks if varofst is already a member of visited *)
          if should_instrument ~visited varofst then (
            visited <- varofst :: visited;
            (* Keeps defs related to this variable *)
            let v_defs =
              Def.Set.filter
                (fun (_, varData, _) -> VarOfst.equal varData varofst)
                defs_set
            in
            let () =
              if not (Def.Set.is_empty v_defs) then (
                if
                  (not (Options.CleanEquiv.get ()))
                  || not (is_equivalent varofst current_stmt kf uses_set)
                (* we're holding some valid defs that we now need to bind the use to via mkSeq *)
                then
                  Def.Set.iter
                    (fun (v_def : Def.t) ->
                      data.du_pairs :=
                        Seq.cons (v_def, current_stmt) !(data.du_pairs);
                      match v_def with
                      | _, _, Kstmt stmt when stmt.sid > current_stmt.sid ->
                          pretty_def Format.err_formatter v_def;
                          pretty_use Format.err_formatter (varofst, current_stmt)
                      | _ -> ())
                    v_defs)
              else
                Format.eprintf
                  "No def found for %a@. This might indicate usage of an \
                   undefined variable, or a global variable that was defined \
                   outside the function.@."
                  Printer.pp_lval (Var v, offset)
            in
            ());
          Cil.DoChildren
      | _ -> Cil.DoChildren
    (** Part 5- a) of the heading comment *)
  end

(******************************)
(***** Dataflow analysis ******)
(******************************)

(** Part 4- a) of the heading comment *)
class visit_use (state : t ref) stmt =
  object (_)
    inherit Visitor.frama_c_inplace
    val mutable visited = []

    method! vexpr expr =
      match !state with
      | Bottom -> Cil.SkipChildren
      | NonBottom (defs, uses) -> (
          match expr.enode with
          | Lval (Var v, offset) ->
              let varofst = (v, trunc_fields offset) in
              if should_instrument ~visited varofst then (
                visited <- varofst :: visited;
                let new_uses = Use.Set.add (varofst, stmt) uses in
                state := NonBottom (defs, new_uses));
              Cil.DoChildren
          | _ -> Cil.DoChildren)
  end

(** Part 4- of the heading comment *)
module Dataflow = struct
  type nonrec t = t

  let pretty_def fmt (defId, (vi, ofst), stmt) =
    let stmt =
      match stmt with
      | Kglobal -> "Function parameter"
      | Kstmt stmt -> Format.asprintf "Stmt %a" Printer.pp_stmt stmt
    in
    Format.fprintf fmt "defId: %d / var : %a / %s@." defId Cil_printer.pp_lval
      (Var vi, ofst) stmt

  let pretty_use fmt (((vi, ofst), stmt) : Use.t) =
    Format.fprintf fmt "var: %a / Stmt: %a@." Printer.pp_lval (Var vi, ofst)
      Printer.pp_stmt stmt

  let pretty_uses fmt set =
    Format.fprintf fmt "  Uses :@.";
    Use.Set.iter
      (fun elt ->
        Format.fprintf fmt "    ";
        pretty_use fmt elt)
      set

  let pretty_defs fmt set =
    Format.fprintf fmt "  Defs :@.";
    Def.Set.iter
      (fun elt ->
        Format.fprintf fmt "    ";
        pretty_def fmt elt)
      set

  let _pretty fmt = function
    | Bottom -> Format.fprintf fmt "Bottom@."
    | NonBottom (defs, uses) ->
        Format.fprintf fmt "NonBottom :@.";
        pretty_defs fmt defs;
        pretty_uses fmt uses

  (** Return a new set after removing all definitions of a given VarOfst *)
  let remove_def varofst s =
    Def.Set.filter
      (fun (_, varData, _) -> not (VarOfst.equal varData varofst))
      s

  (** Return a new set after removing all uses of a given VarOfst *)
  let remove_use varofst s =
    Use.Set.filter (fun (var, _) -> not (VarOfst.equal var varofst)) s

  (** Function called to join 2 states *)
  let join a b =
    match (a, b) with
    | Bottom, x | x, Bottom -> x
    | NonBottom (d, u), NonBottom (d', u') ->
        NonBottom (Def.Set.union d d', Use.Set.inter u u')

  let is_equal a b =
    match (a, b) with
    | Bottom, NonBottom _ | NonBottom _, Bottom -> false
    | Bottom, Bottom -> true
    | NonBottom (d, u), NonBottom (d', u') ->
        Def.Set.equal d d' && Use.Set.equal u u'

  let widen a b =
    let c = join a b in
    if is_equal a c then None else Some c

  (** Part 4- b) of the heading comment *)
  let do_def ?(local = false) v offset stmt = function
    | Bottom -> Some Bottom
    | NonBottom (defs, uses) ->
        let varofst = (v, trunc_fields offset) in
        let defs_clean =
          if local || Options.CleanDataflow.get () then remove_def varofst defs
          else defs
        in
        let uses_clean = remove_use varofst uses in
        let new_defs =
          match Data.lookup_seen_defs data stmt with
          | Some def -> Def.Set.add def defs_clean
          | None ->
              let defId = Data.get_next_def_id data varofst in
              let new_def = (defId, varofst, Kstmt stmt) in
              Data.add_new_def data stmt new_def;
              Def.Set.add new_def defs_clean
        in
        Some (NonBottom (new_defs, uses_clean))

  let rec isConstant e =
    match e.enode with
    | Const _ -> true
    | UnOp (LNot, _, _) -> true
    | UnOp (_, e, _) -> isConstant e
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr), _, _, _) -> true
    | BinOp _ -> false
    | Lval _ -> false
    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> true
    | CastE (_, e) -> isConstant e
    | AddrOf _ | StartOf _ -> false

  (** Function called for each stmt and propagating new states to each succs of
      stmt *)
  let transfer t state =
    let open Interpreted_automata in
    match t with
    | Skip | Prop _ | Enter _ | Leave _ | Return (None, _) -> Some state
    | Return (Some e, stmt) | Guard (e, _, stmt) ->
        let state = ref state in
        ignore (Cil.visitCilExpr (new visit_use state stmt :> Cil.cilVisitor) e);
        Some !state
    | Instr (i, stmt) when not (Utils.is_label i) -> (
        let state = ref state in
        ignore
          (Cil.visitCilInstr (new visit_use state stmt :> Cil.cilVisitor) i);
        match i with
        | Set ((Var vi, offset), _, _) | Call (Some (Var vi, offset), _, _, _)
          ->
            if should_instrument (vi, offset) then do_def vi offset stmt !state
            else Some !state
        | Local_init (vi, _, _) ->
            if should_instrument (vi, NoOffset) then
              do_def ~local:true vi NoOffset stmt !state
            else Some !state
        | Call _ | Set _ | Asm _ | Cil_types.Skip _ | Code_annot _ ->
            Some !state)
    | Instr _ -> Some state
end

(* Dataflow analysis, Part 3-,4-,5- of the heading comment.
 * Performs dataflow analysis on kf and computes the instrumentation that lands in the database.
 * Returns the list of instrumenting statements that should be prepended at the beginning of the function body. *)
let do_function (kf : kernel_function) =
  let module DataflowAnalysis = Interpreted_automata.ForwardAnalysis (Dataflow) in
  (* an obvious part of the named variable set will be the set of formal inputs, which we compute here.
   the rest will be gathered along by the dataflow analysis automaton *)
  let args = Kernel_function.get_formals kf in
  let init_def_set =
    args
    |> List.map (fun arg ->
           let varofst : VarOfst.t = (arg, NoOffset) in
           let defId = Data.get_next_def_id data varofst in
           (defId, varofst, Kglobal))
    |> Def.Set.of_list
  in
  let init_use_set = Use.Set.empty in
  let init_state = NonBottom (init_def_set, init_use_set) in
  (* visits stmt with a visit_defuse visitor - because this is passed to the dataflow analysis result,
   it should only visit a useful subset of statements in deterministic order *)
  let visit_stmt (stmt : stmt) (state : t) =
    match state with
    | Bottom -> ()
    | NonBottom defs_and_uses -> (
        match stmt.skind with
        | Instr i when not (Utils.is_label i) ->
            Cil.visitCilStmt
              (new visit_defuse defs_and_uses stmt kf :> Cil.cilVisitor)
              stmt
            |> ignore
        | Return (Some e, _) | If (e, _, _, _) | Switch (e, _, _, _) ->
            Cil.visitCilExpr
              (new visit_defuse defs_and_uses stmt kf :> Cil.cilVisitor)
              e
            |> ignore
        | _ -> ())
  in
  let result = DataflowAnalysis.fixpoint kf init_state in
  (* __jm__ why is the order important? *)
  DataflowAnalysis.Result.iter_stmt_asc visit_stmt result

type path_bundle = {
  path : int array;
  status_var : varinfo;
  decl_stmt : stmt;
  incr_stmt : stmt;
  label_stmt : stmt;
}

let mk_bundle (mk_label : exp -> 'a list -> location -> stmt) (varname : string)
    (path : int array) : path_bundle =
  let status_varname =
    mk_name ~pre:"__ADUP" varname (Annotators.getCurrentLabelId () + 1)
  in
  let status_var = Cil.makeVarinfo false false status_varname Cil.intType in
  let decl_stmt =
    Local_init
      (status_var, AssignInit (SingleInit (Exp_builder.zero ())), unk_loc)
    |> Stmt_builder.instr
  in

  let lval : lval = (Var status_var, NoOffset) in
  let lval_exp : exp = Exp_builder.lval lval in
  let incr_stmt =
    Ast_info.mkassign lval
      (Exp_builder.binop Cil_types.PlusA lval_exp (Exp_builder.one ()))
      unk_loc
    |> Stmt_builder.instr
  in

  (* __jm__ for developer comfort, the path lengths could be declared 
     in a static const array at the beginning of the function, instead of being magic numbers *)
  let label_stmt =
    mk_label
      (Exp_builder.binop Cil_types.Eq lval_exp
         (Exp_builder.integer (Array.length path)))
      [] unk_loc
  in
  { path; status_var; decl_stmt; incr_stmt; label_stmt }

module PathReducer = struct
  type path = string * int array
  type t = path Seq.t

  let reduce (paths : t) : t =
    paths
    |> Seq.fold_left
         (fun acc named_path ->
           let _, path = named_path in
           let open BatArray in
           match length path with
           | 0 -> assert false
           | k -> (
               let hd = path.(0) in
               acc
               |> List.filter (fun (_, path2) ->
                      let n = length path2 in
                      bsearch BatOrd.poly path2 hd |> function
                      | `At i ->
                          if n - i >= k then equal ( == ) path (sub path2 i k)
                          else false
                      | _ -> false)
               |> List.is_empty
               |> function
               | true -> named_path :: acc
               | false -> acc))
         []
    |> List.rev |> List.to_seq
end

(** Part 1- and 2- of the heading comment *)
class addSequences mk_label (attempt_reduce : bool) =
  object (self)
    inherit Visitor.frama_c_inplace
    val to_add_deactivators = Stmt.Hashtbl.create 17
    val mutable path_bundles : path_bundle list option = None

    (* this method adds the declarations for variables that will be used for instrumentation.
     Perhaps more importantly, it calls do_function to perform dataflow analysis *)
    method! vfunc (dec : fundec) =
      (* __jm__ why is this Option unwrapping done here? how is it known to be safe? *)
      let kf = Option.get self#current_kf in
      if
        Kernel_function.is_definition kf
        && Annotators.shouldInstrumentFun dec.svar
      then (
        (* __jm__ don't know whether this is already computed or not *)
        let first_stmt = Kernel_function.find_first_stmt kf in
        if Graph.detect_cycle first_stmt then (
          Format.printf
            "[lannot] Cycle detected in CFG of %a - this function will not be \
             instrumented.@"
            Printer.pp_fundec dec;
          Cil.SkipChildren)
        else
          (* __jm__ this variable probably holds all declarations of the instrumenting variables,
           I've tested that even variables not scoped to the entire functions land their corresponding instrumenting variables'
           declarations at the beginning of the fn. bodies. This is kind of a flaw in the algorithm, because we could limit the
           amount of instrumentation overhead for unrelated paths if we can avoid this...  
           One solution would be to unify the concept of a block with that of a function, treating a block like an inline closure, 
           kinda, or something like IILE from C++ *)
          let () = do_function kf in
          let rec find_paths (def : stmt) (use : stmt) : int list list =
            (* Format.printf "def: %d, use: %d, comp: %d@." def.sid use.sid
            @@ Int.compare def.sid use.sid; *)
            let open BatOrd in
            match poly def.sid use.sid with
            | Eq -> [ [ def.sid ] ]
            | Lt ->
                def.succs |> List.to_seq
                |> Seq.map (fun s -> find_paths s use)
                |> Seq.map List.to_seq |> Seq.concat
                |> Seq.map (List.cons def.sid)
                |> List.of_seq
            | Gt ->
                Format.printf "Invalid pairs %d %d@." def.sid use.sid;
                []
          in
          let def_to_stmt ((_, _, kinstr) : Def.t) =
            match kinstr with
            | Kglobal -> first_stmt
            | Kstmt def_stmt -> def_stmt
          in
          (* cfg and du_pairs are computed, let's gather the du paths *)
          let paths =
            !(data.du_pairs)
            |> Seq.map (fun (d, u) ->
                   let _, (var, _), _ = d in
                   (var.vname, def_to_stmt d, u))
            |> Seq.map (fun (vname, d, u) ->
                   find_paths d u
                   |> List.map (fun path -> (vname, Array.of_list path)))
            |> Seq.map List.to_seq |> Seq.concat
            |> if attempt_reduce then PathReducer.reduce else Fun.id
          in
          path_bundles <-
            paths
            |> Seq.map (fun (vname, path) -> mk_bundle mk_label vname path)
            |> List.of_seq |> Option.some;

          let cleanup fd =
            Stmt.Hashtbl.reset to_add_deactivators;
            Data.reset_all data;
            Cfg.clearCFGinfo ~clear_id:false fd;
            (* __jm__ why are we computing the CFG again? it's not like we'll be visiting it again *)
            Cfg.cfgFun fd
          in
          Cil.DoChildrenPost
            (fun new_dec ->
              let path_tracking_labels_declarations =
                List.map (fun pb -> pb.decl_stmt) (Option.get path_bundles)
              in
              let pc_labels =
                List.map (fun pb -> pb.label_stmt) (Option.get path_bundles)
              in
              let bstmts_len = List.length new_dec.sbody.bstmts in
              let init, last =
                BatList.split_at (bstmts_len - 1) new_dec.sbody.bstmts
              in
              new_dec.sbody.bstmts <-
                [ path_tracking_labels_declarations; init; pc_labels; last ]
                |> List.flatten;
              cleanup new_dec;
              new_dec))
      else Cil.SkipChildren
    (** Part 1- and 2- of the heading comment *)

    method! vblock _ =
      Cil.DoChildrenPost
        (fun block ->
          block.bstmts <-
            block.bstmts
            |> List.map (fun (stmt : stmt) ->
                   let check_stmt_in_path pb =
                     stmt.sid |> BatArray.bsearch BatOrd.poly pb.path
                     |> function
                     | `At _ -> true
                     | _ -> false
                   in
                   let path_increment_stmts =
                     path_bundles |> Option.get |> List.to_seq
                     |> Seq.filter check_stmt_in_path
                     |> Seq.map (fun pb -> pb.incr_stmt)
                     |> List.of_seq
                   in
                   (* has to be written in such a weird way bc otherwise the return_label disappears, and without Stmt_builder.mk the script crashes with a CFG related error *)
                   let () =
                     stmt.skind <-
                       Block
                         (path_increment_stmts @ [ stmt ]
                         |> List.map (fun stmt -> Stmt_builder.mk stmt.skind)
                         |> Cil.mkBlock)
                   in
                   [ stmt ])
            |> List.flatten;
          block)
    (** Part 2- a) of the heading comment *)

    (* this method populates to_add_deactivators with data from data.def_to_predecessing_defs,
       some assertions about s (the visited statement) clutter this method,
       but it comes down to... I don't know yet __jm__0  *)
    method! vstmt_aux (s : stmt) =
      (* When s is a def, there might be defs done before it whose status labels need to be broken before the def using a deactivator *)
      (* an orthogonal functionality of this method is to, if s.skind is UnspecifiedSequence, use Cil.block_from_unspecified_sequence to force it to be specified *)
      match Data.lookup_predecessing_defs data s with
      (* __jm__ this function uses the results of the analysis coming from def_to_predecessing_defs 
        and they're transfered over to to_add_deactivators  
      *)
      (* __jm__ something to consider when attempting a more widely scoped refactor - to avoid this branching with asserts,
         the def_to_predecessing_defs could be a DS holding a more specific type, with the adding function having a wired-in guard
         that fails when instructions of the wrong type are tried. Right now the same branching is done when adding and fetching
         from the hashtable.
      *)
      | Some def_set -> (
          (* if any predecessing defs were found, that means s is a def also, hence the `assert false` statements in non-conforming branches *)
          match s.skind with
          | Instr i when not (Utils.is_label i) -> (
              match i with
              | Set ((Var _, _), _, _) | Call (Some (Var _, _), _, _, _) ->
                  Def.Set.iter
                    (fun def ->
                      def
                      |> Data.lookup_def_deactivators data
                      |> Option.iter (Stmt_lst.add_list to_add_deactivators s))
                    def_set;
                  Cil.DoChildren
              | _ -> assert false)
          | _ -> assert false)
      | None -> (
          match s.skind with
          | UnspecifiedSequence v ->
              s.skind <- Block (Cil.block_from_unspecified_sequence v);
              Cil.DoChildren
          | _ -> Cil.DoChildren)
    (** Part 2- b) of the heading comment *)
  end

type criterion = ADUP | SDUP

let visited = ref (false, false)
let is_visited _crit = false
let set_visited _crit = ()

(** Visit the file with our main visitor addSequences, and mark the new AST as
    changed *)
let visite file mk_label criterion : unit =
  if false then
    Options.feedback
      "Annotating with 2 dataflow criteria at the same time is not supported"
  else
    Visitor.visitFramacFileSameGlobals
      (new addSequences
         mk_label
         (match criterion with ADUP -> false | SDUP -> true)
        :> Visitor.frama_c_visitor)
      file

let is_bounded = function _ -> false
let apply_factory criterion mk_label file = visite file mk_label criterion

module ADUP = Annotators.Register (struct
  let name = "ADUP"
  let help = "All-DU-Paths Coverage"
  let apply = apply_factory ADUP
end)

module SDUP = Annotators.Register (struct
  let name = "SDUP"
  let help = "Some-DU-Paths Coverage"
  let apply = apply_factory SDUP
end)
