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

    a) to_add_defs, to_add_uses, to_add_conds and to_add_fun are used to place
    created def/use from visit_defuse

    b) If the statement is a Def of V * find in to_add_conds_stmt all preceding
    defs of V * for each def, find in def_to_conds all conditions for this def *
    Remember these conditions for this statement in to_add_conds Data flow
    analysis (do_function) :

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

    b) If this statement is a Def of V, we remember in to_add_conds_stmt all
    defs of V that can reach this statements, which will be used in addSequences
    to create condition statements, i.e. reset of status variables *)

(** Used to represent a Lval with a truncated offset (trunc at first Index) *)
module VarOfst =
  Datatype.Pair_with_collections (Varinfo) (Offset)
    (struct
      let module_name = "L_dataflow.VarOfst"
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
      let module_name = "L_dataflow.Def"
    end)

(** Used to represent a use *)
module Use =
  Datatype.Pair_with_collections
    (VarOfst)
    (* Store varinfo and truncated offset of this use  *)
    (Stmt)
    (* Use's statement *)
    (struct
      let module_name = "L_dataflow.Use"
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

(** For each VarOfst, keep the number of assignment seen for this VarOfst key :
    VarOfst.t value : int *)
let def_id = VarOfst.Hashtbl.create 32

(** each def is registered to avoid creating them more than once key : stmt
    value : Def.t *)
let seen_def = Stmt.Hashtbl.create 32

let postpone_def = Def.Hashtbl.create 32

(** bind each new sequence to its corresponding statement, sequence id (int) is
    used to sort them at the end key : statement value : (int * stmt) list *)
let to_add_defs = Stmt.Hashtbl.create 32

let to_add_uses = Stmt.Hashtbl.create 32
let to_add_conds_stmt = Stmt.Hashtbl.create 32

(** Bind to each def the list of conditions related to it key : Def.t value :
    (int * stmt) list *)
let def_to_conds = Def.Hashtbl.create 32

(** Used to create hyperlabels, bind to each def the list of sequences related
    to it key : Def.t value : int list *)
let hyperlabels = Def.Hashtbl.create 32

(** Clear all hashtbls after each function in addSequences *)
let reset_all () =
  VarOfst.Hashtbl.reset def_id;
  Stmt.Hashtbl.reset seen_def;
  Stmt.Hashtbl.reset to_add_defs;
  Stmt.Hashtbl.reset to_add_uses;
  Stmt.Hashtbl.reset to_add_conds_stmt;
  Def.Hashtbl.reset def_to_conds

(** Given a VarOfst, increment the number of defs seen and returns it. If it is
    the first return 1. *)
let get_next_def_id varofst =
  match VarOfst.Hashtbl.find_opt def_id varofst with
  | None ->
      VarOfst.Hashtbl.add def_id varofst 1;
      1
  | Some n ->
      let new_val = n + 1 in
      VarOfst.Hashtbl.replace def_id varofst new_val;
      new_val

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
    consider 1 use of v1. Method works by looking for varofst in visited *)
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
class visit_defuse ~annot_bound (defs_set, uses_set) current_stmt kf
  (mk_label : exp -> 'a list -> location -> stmt) to_add_fun =
  object (self)
    inherit Visitor.frama_c_inplace

    val mutable visited = []
    (** Used to avoid trivially equivalent uses *)

    val mutable visited_stmt = false

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

    method private register_seq def (sequence_id : int)
        (to_register : (varinfo * exp) list) (cond : stmt) (use_lbl : stmt) =
      let _, _, (stmtDef : kinstr) = def in
      let iter_f =
        match stmtDef with
        | Kglobal ->
            fun (vInfo, def_exp) ->
              let init = self#mk_init vInfo def_exp in
              to_add_fun := (sequence_id, init) :: !to_add_fun
        | Kstmt stmt ->
            fun (vInfo, def_exp) ->
              let init = self#mk_init vInfo (Exp_builder.zero ()) in
              to_add_fun := (sequence_id, init) :: !to_add_fun;
              Stmt_lst.add to_add_defs stmt
                (sequence_id, self#mk_set vInfo def_exp)
      in
      List.iter iter_f to_register;
      let bound_cond = (sequence_id, cond) in
      let bound_use_lbl = (sequence_id, use_lbl) in
      Def_lst.add def_to_conds def bound_cond;
      Stmt_lst.add to_add_uses current_stmt bound_use_lbl;
      Def_lst.add hyperlabels def sequence_id
    (** Register in Hashtbls the different parts of our sequences (Def / Use /
        Cond) Plus the initialization (depending on the type of stmtDef, cf. Def
        type) and add this sequence id to its hyperlabel *)

    method private mkSeq_aux loc def bound =
      let _, (vi, offset), _ = def in
      (* sequence id *)
      let id_seq = Annotators.getCurrentLabelId () + 1 in
      let vInfo = self#init_vinfo (mk_name vi.vname id_seq) in
      let use = self#mk_comp vInfo (Exp_builder.one ()) in
      let use, to_register =
        match (bound, Options.BoundPostpone.get ()) with
        | None, _ -> (use, [ (vInfo, Exp_builder.one ()) ])
        | Some (op, bound), true ->
            let tmp_vInfo, init =
              if Def.Hashtbl.mem postpone_def def then
                (Def.Hashtbl.find postpone_def def, [])
              else
                let tmp_vInfo =
                  self#init_vinfo
                    ~typ:
                      (Cil.typeRemoveAttributes [ "const" ]
                      @@ Cil.typeOfLval (Var vi, offset))
                    (mk_name ~pre:seq_tmp_prefix vi.vname id_seq)
                in
                Def.Hashtbl.add postpone_def def tmp_vInfo;
                let exp_vInfo = Exp_builder.mk (Lval (Var vi, offset)) in
                (tmp_vInfo, [ (tmp_vInfo, exp_vInfo) ])
            in
            let pred = self#mk_comp ~op tmp_vInfo bound in
            ( Exp_builder.binop Cil_types.LAnd use pred,
              [ (vInfo, Exp_builder.one ()) ] @ init )
        | Some (op, bound), false ->
            (use, [ (vInfo, self#mk_comp ~offset ~op vi bound) ])
      in
      let break_seq = self#mk_set vInfo (Exp_builder.zero ()) in
      let use_lbl = mk_label use [] loc in
      self#register_seq def id_seq to_register break_seq use_lbl
    (** Create a def-use sequence for the given def/bounds and register it in
        Hashtbls *)

    method private mkSeq loc def =
      let _, (vi, offset), _ = def in
      match Cil.typeOfLval (Var vi, offset) |> Cil.unrollType with
      | (TInt (kind, _) | TEnum ({ ekind = kind }, _)) when annot_bound ->
          Utils.get_bounds kind
          |> List.iter (fun b -> self#mkSeq_aux loc def (Some b))
      | _ when annot_bound -> ()
      | _ -> self#mkSeq_aux loc def None
    (** Create a def-use sequence for the given def and bounds *)

    method! vstmt_aux stmt =
      (* ensure this method is called only once *)
      Format.eprintf "vstmt_aux %a@." Printer.pp_stmt stmt;
      assert (Stmt.equal stmt current_stmt);
      assert (not visited_stmt);
      visited_stmt <- true;
      match stmt.skind with
      | Instr i when not (Utils.is_label i) -> (
          match i with
          | Set ((Var v, offset), _, _) | Call (Some (Var v, offset), _, _, _)
            ->
              let varofst = (v, trunc_fields offset) in
              (if should_instrument varofst then
                 let t =
                   Def.Set.filter
                     (fun (_, varData, _) -> VarOfst.equal varData varofst)
                     defs_set
                 in
                 if not @@ Def.Set.is_empty t then
                   Stmt.Hashtbl.add to_add_conds_stmt stmt t);
              Cil.DoChildren
          | _ -> Cil.DoChildren)
      | _ -> assert false
    (** Part 5- b) of the heading comment *)

    (* visits an AST expression node and it's children recursively. Ignores expressions that are not Lvalues.
       Meant to identify all lval uses in an expression.

    *)
    method! vexpr expr =
      match expr.enode with
      | Lval (Var v, offset) ->
          let varofst = (v, trunc_fields offset) in
          if should_instrument ~visited varofst then (
            visited <- varofst :: visited;
            (* Keeps defs related to this variable *)
            let all_varofst_defs =
              Def.Set.filter
                (fun (_, varData, _) -> VarOfst.equal varData varofst)
                defs_set
            in
            if not (Def.Set.is_empty all_varofst_defs) then
              if
                (not (Options.CleanEquiv.get ()))
                || not (is_equivalent varofst current_stmt kf uses_set)
              then Def.Set.iter (self#mkSeq expr.eloc) all_varofst_defs);
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
module Dataflow (B : sig
  val bounded : bool
end) =
struct
  type nonrec t = t

  let pretty_def fmt (defId, (vi, ofst), stmt) =
    let stmt =
      match stmt with
      | Kglobal -> "Function parameter"
      | Kstmt stmt -> Format.asprintf "Stmt %a" Printer.pp_stmt stmt
    in
    Format.fprintf fmt "defId: %d / var : %a / %s@." defId Cil_printer.pp_lval
      (Var vi, ofst) stmt

  let pretty_use fmt ((vi, ofst), stmt) =
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
          match Stmt.Hashtbl.find_opt seen_def stmt with
          | Some def -> Def.Set.add def defs_clean
          | None ->
              let defId = get_next_def_id varofst in
              let new_def = (defId, varofst, Kstmt stmt) in
              Stmt.Hashtbl.add seen_def stmt new_def;
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
        | (Set (_, e, _) | Local_init (_, AssignInit (SingleInit e), _))
          when isConstant e && B.bounded ->
            Some !state
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

(** Dataflow analysis, Part 3-,4-,5- of the heading comment *)
let do_function ~annot_bound (kf : kernel_function) mk_label to_add_fun =
  let module DataflowAnalysis =
  Interpreted_automata.ForwardAnalysis (Dataflow (struct
    let bounded = annot_bound
  end)) in
  (* an obvious part of the named variable set will be the set of formal inputs, which we compute here.
   the rest will be gathered along by the dataflow analysis automaton *)
  let args = Kernel_function.get_formals kf in
  let init_def_set =
    args
    |> List.map (fun arg ->
           let varofst : VarOfst.t = (arg, NoOffset) in
           let defId = get_next_def_id varofst in
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
    | NonBottom t -> (
        match stmt.skind with
        | Instr i when not (Utils.is_label i) ->
            Cil.visitCilStmt
              (new visit_defuse ~annot_bound t stmt kf mk_label to_add_fun
                :> Cil.cilVisitor)
              stmt
            |> ignore
        | Return (Some e, _) | If (e, _, _, _) | Switch (e, _, _, _) ->
            Cil.visitCilExpr
              (new visit_defuse ~annot_bound t stmt kf mk_label to_add_fun
                :> Cil.cilVisitor)
              e
            |> ignore
        | _ -> ())
  in
  let result = DataflowAnalysis.fixpoint kf init_state in
  DataflowAnalysis.Result.iter_stmt_asc visit_stmt result

(** Part 1- and 2- of the heading comment *)
class addSequences ~annot_bound mk_label =
  object (self)
    inherit Visitor.frama_c_inplace
    val to_add_conds = Stmt.Hashtbl.create 17

    (* get all sequences, sort defs and uses by sequence ID, and returns
     a pair of list (before,after) with sequences to add before & after the current statement *)
    (* __jm__ I think stmt actually signifies an lval variable here,
     if were extracting all corresponding uses (which includes conds) and defs *)
    (* __jm__ after further inspection, these are probably the instrumentation lines to be added
     *)
    method private get_seqs_sorted stmt =
      let sort = List.sort compare in
      let find_or_empty hashtable key =
        Stmt.Hashtbl.find_def hashtable key []
      in
      let defs = stmt |> find_or_empty to_add_defs |> sort |> List.map snd in
      let uses = stmt |> find_or_empty to_add_uses |> sort |> List.map snd in
      let conds =
        stmt |> find_or_empty to_add_conds |> sort
        |> List.map (fun (_, s) -> Stmt_builder.mk s.skind)
      in
      (uses @ conds, defs)

    (* this method adds the declarations for variables that will be used for instrumentation.
     Perhaps more importantly, it calls do_function to perform dataflow analysis *)
    method! vfunc (dec : fundec) =
      (* __jm__ why is this Option unwrapping done here? how is it known to be safe? *)
      let kf = Option.get self#current_kf in
      if
        Kernel_function.is_definition kf
        && Annotators.shouldInstrumentFun dec.svar
      then (
        let instrumented_vars_declarations = ref [] in
        do_function ~annot_bound kf mk_label instrumented_vars_declarations;

        let cleanup dec' =
          Stmt.Hashtbl.reset to_add_conds;
          reset_all ();
          Cfg.clearCFGinfo ~clear_id:false dec';
          (* __jm__ why are we computing the CFG again? it's not like we'll be visiting it again *)
          Cfg.cfgFun dec'
        in

        Cil.DoChildrenPost
          (fun new_dec ->
            let ivd_stmts =
              !instrumented_vars_declarations
              |> List.sort compare |> List.map snd
            in
            new_dec.sbody.bstmts <- ivd_stmts @ new_dec.sbody.bstmts;
            cleanup new_dec;
            new_dec))
      else Cil.SkipChildren
    (** Part 1- and 2- of the heading comment *)

    (* this method simply fetches statements to be added using get_seqs_sorted 
     and then tries adding them to the source *)
    method! vblock _ =
      Cil.DoChildrenPost
        (fun block ->
          block.bstmts <-
            block.bstmts
            |> List.map (fun (stmt : stmt) ->
                   let before, after = self#get_seqs_sorted stmt in
                   let () =
                     stmt.skind <-
                       Block
                         (Cil.mkBlock @@ before @ [ Stmt_builder.mk stmt.skind ])
                   in
                   stmt :: after)
            |> List.flatten;
          block)
    (** Part 2- a) of the heading comment *)

    (* this method deals with adding pc_label* statements *)
    method! vstmt_aux (s : stmt) =
      (* if there are pc_label statements (in to_add_conds_stmt) to be added before s, add them *)
      (* otherwise, if s.skind is UnspecifiedSequence, Frama-C will use Cil.block_from_unspecified_sequence to force it to be specified *)
      match Stmt.Hashtbl.find_opt to_add_conds_stmt s with
      | Some def_set -> (
          match s.skind with
          | Instr i when not (Utils.is_label i) -> (
              match i with
              | Set ((Var _, _), _, _) | Call (Some (Var _, _), _, _, _) ->
                  Def.Set.iter
                    (fun def ->
                      def
                      |> Def.Hashtbl.find_opt def_to_conds
                      |> Option.iter (fun conds ->
                             Stmt_lst.add_list to_add_conds s conds))
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

type criterion = ADC | AUC | DUC | BADC | BAUC | BDUC

let operator_of_criterion crit =
  match crit with
  | ADC | BADC -> "+"
  | AUC | BAUC -> "."
  | DUC | BDUC -> assert false

(** Create all hyperlabels *)
let compute_hl crit =
  match crit with
  | DUC | BDUC ->
      Def.Hashtbl.fold_sorted
        (fun _ seqs str ->
          let seqs = List.sort compare seqs in
          List.fold_left
            (fun acc s ->
              acc ^ Annotators.next_hl () ^ ") <s" ^ string_of_int s
              ^ "|; ;>,\n")
            str seqs)
        hyperlabels ""
  | _ ->
      let symb = operator_of_criterion crit in
      Def.Hashtbl.fold_sorted
        (fun _ seqs str ->
          let seqs = List.sort compare seqs in
          str ^ Annotators.next_hl () ^ ") <"
          ^ String.concat symb (List.map (fun s -> "s" ^ string_of_int s) seqs)
          ^ "|; ;>,\n")
        hyperlabels ""

(** Generate/Append in .hyperlabels file our hyperlabels data *)
let gen_hyperlabels crit =
  let data_filename =
    Filename.chop_extension (Annotators.get_file_name ()) ^ ".hyperlabels"
  in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl crit in
  let out = open_out_gen [ Open_creat; Open_append ] 0o644 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of hyperlabels = %d"
    (Annotators.getCurrentHLId ())

let visited = ref (false, false)

let is_visited crit =
  match (crit, !visited) with
  | (ADC | AUC | DUC), (v, _) -> v
  | (BADC | BAUC | BDUC), (_, v) -> v

let set_visited crit =
  match (crit, !visited) with
  | (ADC | AUC | DUC), (_, old) -> visited := (true, old)
  | (BADC | BAUC | BDUC), (old, _) -> visited := (old, true)

(** Visit the file with our main visitor addSequences, and mark the new AST as
    changed *)
let visite ?(annot_bound = false) crit file mk_label : unit =
  if is_visited crit then
    Options.feedback
      "Annotating with 2 dataflow criteria at the same time is not supported"
  else (
    Visitor.visitFramacFileSameGlobals
      (new addSequences ~annot_bound mk_label :> Visitor.frama_c_visitor)
      file;
    set_visited crit)

let is_bounded = function
  | BADC | BAUC | BDUC -> true
  | ADC | AUC | DUC -> false

let apply_factory crit mk_label file =
  visite ~annot_bound:(is_bounded crit) crit file mk_label;
  gen_hyperlabels crit

(** All-defs annotator *)
module ADC = Annotators.Register (struct
  let name = "ADC"
  let help = "All-Definitions Coverage"
  let apply = apply_factory ADC
end)

(** All-uses annotator *)
module AUC = Annotators.Register (struct
  let name = "AUC"
  let help = "All-Uses Coverage"
  let apply = apply_factory AUC
end)

(** Def-Use annotator *)
module DUC = Annotators.Register (struct
  let name = "DUC"
  let help = "Definition-Use Coverage"
  let apply = apply_factory DUC
end)

(** Boundary All-defs annotator *)
module BADC = Annotators.Register (struct
  let name = "BADC"
  let help = "Boundary All-Definitions Coverage"
  let apply = apply_factory BADC
end)

(** Boundary All-uses annotator *)
module BAUC = Annotators.Register (struct
  let name = "BAUC"
  let help = "Boundary All-Uses Coverage"
  let apply = apply_factory BAUC
end)

(** Boundary Def-Use annotator *)
module BDUC = Annotators.Register (struct
  let name = "BDUC"
  let help = "Boundary Definition-Use Coverage"
  let apply = apply_factory BDUC
end)
