(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2020                                               *)
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
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype
open Ast_const

(** This file is used to generate dataflow criteria and is structured this way :
    The main AST visitor is addSequences :
    1- For each function, before visiting it, we perform a dataflow analysis
      (cf. dataflow analysis part).
    2- After the dataflow analysis, we visit the function, and add all annotations
      created by the dataflow analysis (Def / Use / Status variable)
      a) to_add_defs, to_add_uses, to_add_conds and to_add_fun are used to place
         created def/use from visit_defuse
      b) If the statement is a Def of V
        * find in to_add_conds_stmt all preceding defs of V
        * for each def, find in def_to_conds all conditions for this def
        * Remember these conditions for this statement in to_add_conds

    Data flow analysis (do_function) :
    3- Initialize the initial state for the analysis (containing definitions from
      function's parameters)
    4- Perform the analysis by propagating a state (Defs set, Uses set) :
      a) Visit all expressions to find all uses in the function, and put them in
        the set Uses in our state. Join with intersection.
      b) Visit all definitions to find all defs in the function, and put them in
        the set Defs in our state. Join with union.
        When computing a Def of V, remove V from Uses, and remove V from Defs if
        CleanDataflow is true
    5- Then we iterate on all statements with their corresponding
      state, and visit (visit_defuse) this statement.
        a) For each use of V, we create a sequence for each Def of V is our
          state
        b) If this statement is a Def of V, we remember in to_add_conds_stmt
          all defs of V that can reach this statements, which will be used in
          addSequences to create condition statements, i.e. reset of status
          variables

*)

(** Used to represent a Lval with a truncated offset (trunc at first Index) *)
module VarOfst =
  Datatype.Pair_with_collections
    (Varinfo)
    (Offset)
    (struct let module_name = "L_dataflow.VarOfst" end)

(** Used to represent a definition *)
module Def =
  Datatype.Triple_with_collections
    (Datatype.Int) (* Unique ID for each different definition of VarOfst *)
    (VarOfst)      (* Store varinfo and truncated offset of this definition *)
    (Kinstr)       (* KGlobal for function's parameters, KStmt otherwise *)
    (struct let module_name = "L_dataflow.Def" end)

(** Used to represent a use *)
module Use =
  Datatype.Pair_with_collections
    (VarOfst)      (* Store varinfo and truncated offset of this use  *)
    (Stmt)         (* Use's statement *)
    (struct let module_name = "L_dataflow.Use" end)

(** Module to add 2 tool functions *)
module Listtbl (T : Hashtbl.S) = struct
  include T

  (** Add an element in front of the existing binding, else bind to a new list
      containing this element *)
  let add tbl key elt =
    if T.mem tbl key then
      let old = T.find tbl key in
      T.replace tbl key (elt::old)
    else
      T.add tbl key [elt]

  (** Add a list in front of the existing binding, else create a new binding *)
  let add_list tbl key elt =
    if T.mem tbl key then
      let old = T.find tbl key in
      T.replace tbl key (elt@old)
    else
      T.add tbl key elt
end

module VarOfst_lst = Listtbl(VarOfst.Hashtbl)
module Stmt_lst = Listtbl(Stmt.Hashtbl)
module Def_lst = Listtbl(Def.Hashtbl)

(** Represent states in our dataflow analysis *)
type t =
  | Bottom
  | NonBottom of (Def.Set.t * Use.Set.t)

(** For each VarOfst, keep the number of assignment seen for this VarOfst
    key : VarOfst.t
    value : int
*)
let def_id = VarOfst.Hashtbl.create 32

(** each def is registered to avoid creating them more than once
    key : stmt
    value : Def.t
*)
let seen_def = Stmt.Hashtbl.create 32

let postpone_def = Def.Hashtbl.create 32


(** bind each new sequence to its corresponding statement, sequence id (int) is
    used to sort them at the end
    key : statement
    value : (int * stmt) list
*)
let to_add_defs = Stmt.Hashtbl.create 32
let to_add_uses = Stmt.Hashtbl.create 32
let to_add_conds_stmt = Stmt.Hashtbl.create 32

(** Bind to each def the list of conditions related to it
    key : Def.t
    value : (int * stmt) list
*)
let def_to_conds = Def.Hashtbl.create 32

(** Used to create hyperlabels, bind to each def the list of sequences related to it
    key : Def.t
    value : int list
*)
let hyperlabels = Def.Hashtbl.create 32

(** Clear all hashtbls after each function in addSequences *)
let reset_all () =
  VarOfst.Hashtbl.reset def_id;
  Stmt.Hashtbl.reset seen_def;
  Stmt.Hashtbl.reset to_add_defs;
  Stmt.Hashtbl.reset to_add_uses;
  Stmt.Hashtbl.reset to_add_conds_stmt;
  Def.Hashtbl.reset def_to_conds

(** Given a VarOfst, increment the number of defs seen
    and returns it. If it is the first return 1. *)
let get_next_def_id varofst =
  if VarOfst.Hashtbl.mem def_id varofst then
    let n = (VarOfst.Hashtbl.find def_id varofst) + 1 in
    VarOfst.Hashtbl.replace def_id varofst n;
    n
  else
    (VarOfst.Hashtbl.add def_id varofst 1; 1)

(** Given an offset, truncate it before the first Index
    var->field1->filed2[42]->field3
    will become
    var->field1->field2
*)
let rec trunc_fields offset =
  match offset with
  | Field (f_info,offset') ->
    let offset' = trunc_fields offset' in
    Field (f_info,offset')
  | Index _ | NoOffset -> NoOffset

(** For a given VarOfst V, statement S1 and set of Uses (from dataflow analysis state)
    Try to find a match (V', S2) in Uses such that
    V equals V' and S1 post-dominate S2
*)
let is_equivalent varofst stmt kf uses =
  Use.Set.exists (fun (varofst',stmtUse) ->
      let eq = VarOfst.equal varofst' varofst in
      (* Our dataflow analysis garanty that uses in state always dominate the
         current statement S1 *)
      if eq && not (Dominators.dominates stmtUse stmt) then
        Options.fatal "Discrepancy: This should not happen";
      eq &&
      !Db.Postdominators.is_postdominator kf ~opening:stmtUse ~closing:stmt
    ) uses

(** Consider uses of V in an expression only once.
    In v2 = v1 + v1, we will consider 1 use of v1
*)
let is_triv_equiv varofst visited =
  Options.CleanEquiv.get() &&
  List.exists (fun varofst' -> VarOfst.equal varofst' varofst) visited

let seq_status_prefix = "__SEQ_STATUS"
let seq_tmp_prefix = "__SEQ_TMP"

let mk_name ?(pre=seq_status_prefix) s id = pre ^ "_" ^ s ^ "_" ^ string_of_int id

(** Test if a VarOfst V should be instrumented, i.e. if it's not a global,
    a temp variable or if it's the first time we see it in the current
    expr/instr (except if CleanDuplicate is false) *)
let should_instrument ?visited (v,offset) =
  not v.vglob && not v.vtemp
  && not (Datatype.String.equal v.vname "__retres")
  && not @@ Extlib.string_prefix seq_status_prefix v.vname
  && Annotators.shouldInstrumentVar v
  && match visited with
  | None -> true
  | Some visited -> not (is_triv_equiv (v,offset) visited)

let unk_loc = Location.unknown

(***********************************)
(********* Defuse Criteria *********)
(***********************************)

(** Perform the part 5- of the heading comment *)
class visit_defuse ~annot_bound (defs_set,uses_set) current_stmt kf
    (mk_label: exp -> 'a list -> location -> stmt) to_add_fun = object(self)
  inherit Visitor.frama_c_inplace

  (** Used to avoid trivially equivalent uses *)
  val mutable visited = []

  (** Create a new varinfo *)
  method private init_vinfo ?(typ=Cil.intType) name =
    Cil.makeVarinfo false false name typ

  (** Create a statement : vi = value; *)
  method private mk_set ?(loc=unk_loc) vi value =
    Ast_info.mkassign (Var vi, NoOffset) value loc
    |> Stmt_builder.instr

  (** Create a expression : Lval(vi,offset) == value *)
  method private mk_comp ?(loc=unk_loc) ?(op=Cil_types.Eq) ?(offset=NoOffset) vi value =
    let new_exp = Exp_builder.mk ~loc (Lval (Var vi, offset)) in
    Exp_builder.binop op new_exp value

  (** Create a statement : typ vi = value; where typ is the type of vi *)
  method private mk_init ?(loc=unk_loc) vi value =
    Local_init(vi,AssignInit(SingleInit(value)),loc)
    |> Stmt_builder.instr

  (** Register in Hashtbls the different parts of our sequences (Def / Use / Cond)
      Plus the initialization (depending on the type of stmtDef, cf. Def type)
      and add this sequence id to its hyperlabel
  *)
  method private register_seq def id_seqs to_register cond use_lbl =
    let _,_,stmtDef = def in
    List.iter (fun (vInfo,def_exp) ->
        match stmtDef with
        | Kglobal ->
          let init = self#mk_init vInfo def_exp in
          to_add_fun := (id_seqs, init) :: !to_add_fun
        | Kstmt stmt ->
          let init = self#mk_init vInfo (Exp_builder.zero ()) in
          to_add_fun := (id_seqs, init) :: !to_add_fun;
          Stmt_lst.add to_add_defs stmt (id_seqs, self#mk_set vInfo def_exp)
      ) to_register;
    Def_lst.add def_to_conds def (id_seqs,cond);
    Stmt_lst.add to_add_uses current_stmt (id_seqs,use_lbl);
    Def_lst.add hyperlabels def id_seqs

  (** Create a def-use sequence for the given def/bounds
      and register it in Hashtbls
  *)
  method private mkSeq_aux loc def bound =
    let _,(vi,offset),_ = def in
    let id_seq = Annotators.getCurrentLabelId () + 1 in (* sequence id *)
    let vInfo = self#init_vinfo (mk_name vi.vname id_seq) in
    let use = self#mk_comp vInfo (Exp_builder.one ()) in
    let use,to_register =
      match bound, Options.BoundPostpone.get () with
      | None, _ -> use,[vInfo, Exp_builder.one ()]
      | Some (op,bound), true ->
        let tmp_vInfo, init =
          if Def.Hashtbl.mem postpone_def def then
            Def.Hashtbl.find postpone_def def, []
          else begin
            let tmp_vInfo = self#init_vinfo ~typ:(Cil.typeOfLval (Var vi, offset))
                (mk_name ~pre:seq_tmp_prefix vi.vname id_seq)
            in
            Def.Hashtbl.add postpone_def def tmp_vInfo;
            let exp_vInfo = Exp_builder.mk (Lval (Var vi, offset)) in
            tmp_vInfo, [(tmp_vInfo, exp_vInfo)]
          end
        in
        let pred = self#mk_comp ~op tmp_vInfo bound in
        Exp_builder.binop Cil_types.LAnd use pred,
        [(vInfo,Exp_builder.one ())] @ init
      | Some (op,bound), false ->
        use,
        [(vInfo,self#mk_comp ~offset ~op vi bound)]
    in
    let break_seq = self#mk_set vInfo (Exp_builder.zero ()) in
    let use_lbl = mk_label use [] loc in
    self#register_seq def id_seq to_register break_seq use_lbl

  (** Create a def-use sequence for the given def and bounds *)
  method private mkSeq loc def =
    let _,(vi,offset),_ = def in
    match Cil.typeOfLval (Var vi, offset) |> Cil.unrollType with
    | TInt (kind, _)
    | TEnum ({ekind = kind}, _) ->
      if annot_bound then
        Utils.get_bounds kind
        |> List.iter (fun b -> self#mkSeq_aux loc def (Some b))
      else self#mkSeq_aux loc def None
    | _ -> self#mkSeq_aux loc def None

  (** Part 5- b) of the heading comment *)
  method! vstmt_aux stmt =
    match stmt.skind with
    | Instr i when not (Utils.is_label i) ->
      begin match i with
        | Set ((Var v,offset),_,_)
        | Call (Some (Var v,offset),_,_,_) ->
          let varofst = (v,trunc_fields offset) in
          if should_instrument varofst then begin
            let t = Def.Set.filter (fun (_,varData,_) ->
                VarOfst.equal varData varofst) defs_set in
            if not @@ Def.Set.is_empty t then
              Stmt.Hashtbl.add to_add_conds_stmt stmt t;
          end;
          Cil.DoChildren
        | _ -> Cil.DoChildren
      end
    | _ -> assert false

  (** Part 5- a) of the heading comment *)
  method! vexpr expr =
    match expr.enode with
    | Lval (Var v, offset) ->
      let varofst = (v,trunc_fields offset) in
      if should_instrument ~visited varofst then begin
        visited <- varofst :: visited;
        (* Keeps defs related to this variable *)
        let all_varofst_defs =
          Def.Set.filter (fun (_,varData,_) ->
              VarOfst.equal varData varofst) defs_set
        in
        if not (Def.Set.is_empty all_varofst_defs) then
          if not (Options.CleanEquiv.get ())
          || not (is_equivalent varofst current_stmt kf uses_set) then
            Def.Set.iter (self#mkSeq expr.eloc) all_varofst_defs;
      end;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

(******************************)
(***** Dataflow analysis ******)
(******************************)

(** Part 4- a) of the heading comment *)
class visit_use state stmt = object(_)
  inherit Visitor.frama_c_inplace
  val mutable visited = []

  method! vexpr expr =
    match !state with
    | Bottom -> Cil.SkipChildren
    | NonBottom (defs,uses) ->
      match expr.enode with
      | Lval (Var v, offset) ->
        let varofst = (v,trunc_fields offset) in
        if should_instrument ~visited varofst then begin
          visited <- varofst :: visited;
          let new_uses = Use.Set.add (varofst,stmt) uses in
          state := NonBottom (defs, new_uses);
        end;
        Cil.DoChildren
      | _ -> Cil.DoChildren
end

(** Part 4- of the heading comment *)
module Dataflow (B : sig val bounded : bool end) = struct

  type nonrec t = t

  let pretty_def fmt (defId,(vi,ofst),stmt) =
    let stmt =
      match stmt with
      | Kglobal -> "Function parameter"
      | Kstmt stmt -> Format.asprintf "Stmt %a" Printer.pp_stmt stmt
    in
    Format.fprintf fmt "defId: %d / var : %a / %s@."
      defId Cil_printer.pp_lval (Var vi, ofst) stmt

  let pretty_use fmt ((vi,ofst),stmt) =
    Format.fprintf fmt "var: %a / Stmt: %a@."
      Printer.pp_lval (Var vi, ofst) Printer.pp_stmt stmt

  let pretty_uses fmt set =
    Format.fprintf fmt "  Uses :@.";
    Use.Set.iter (fun elt -> Format.fprintf fmt "    "; pretty_use fmt elt) set

  let pretty_defs fmt set =
    Format.fprintf fmt "  Defs :@.";
    Def.Set.iter (fun elt -> Format.fprintf fmt "    "; pretty_def fmt elt) set

  let pretty fmt = function
    | Bottom -> Format.fprintf fmt "Bottom@."
    | NonBottom (defs,uses) ->
      Format.fprintf fmt "NonBottom :@.";
      pretty_defs fmt defs;
      pretty_uses fmt uses

  (** Return a new set after removing all definitions of a given VarOfst *)
  let remove_def varofst s =
    Def.Set.filter (fun (_,varData,_) -> not (VarOfst.equal varData varofst)) s

  (** Return a new set after removing all uses of a given VarOfst *)
  let remove_use varofst s =
    Use.Set.filter (fun (var,_) -> not (VarOfst.equal var varofst)) s

  (** Function called to join 2 states *)
  let join a b =
    match a,b with
    | Bottom, x | x, Bottom -> x
    | NonBottom (d,u), NonBottom (d',u') ->
      NonBottom (Def.Set.union d d',Use.Set.inter u u')

  let is_equal a b =
    match a, b with
    | Bottom, NonBottom _
    | NonBottom _, Bottom -> false
    | Bottom, Bottom -> true
    | NonBottom (d,u), NonBottom (d',u') ->
      Def.Set.equal d d' && Use.Set.equal u u'

  let widen a b =
    let c = join a b in
    if is_equal a c then None
    else Some c

  (** Part 4- b) of the heading comment *)
  let do_def ?(local=false) v offset stmt = function
    | Bottom -> Some Bottom
    | NonBottom (defs,uses) ->
      let varofst = (v,trunc_fields offset) in
      let defs_clean =
        if local || Options.CleanDataflow.get () then
          remove_def varofst defs
        else
          defs
      in
      let uses_clean = remove_use varofst uses in
      let new_defs =
        if Stmt.Hashtbl.mem seen_def stmt then
          Def.Set.add (Stmt.Hashtbl.find seen_def stmt) defs_clean
        else begin
          let defId = get_next_def_id varofst in
          let new_def = (defId, varofst, (Kstmt stmt)) in
          Stmt.Hashtbl.add seen_def stmt new_def;
          Def.Set.add new_def defs_clean
        end
      in
      Some (NonBottom (new_defs,uses_clean))

  let rec isConstant e =
    match e.enode with
    | Info _ -> assert false
    | Const _ -> true
    | UnOp (LNot, _, _) -> true
    | UnOp (_, e, _) -> isConstant e
    | BinOp ((Lt|Gt|Le|Ge|Eq|Ne|LAnd|LOr), _, _, _) -> true
    | BinOp _ -> false
    | Lval _ -> false
    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> true
    | CastE (_, e) -> isConstant e
    | AddrOf _ | StartOf _ -> false

  (** Function called for each stmt and propagating new states to each succs of
      stmt
  *)
  let transfer t state =
    let open Interpreted_automata in
    match t with
    | Skip | Prop _ | Enter _ | Leave _ | Return (None,_) -> Some state
    | Return (Some e, stmt)
    | Guard (e, _, stmt) ->
      let state = ref state in
      ignore(Cil.visitCilExpr (new visit_use state stmt :> Cil.cilVisitor) e);
      Some !state
    | Instr (i, stmt) when not (Utils.is_label i) ->
      let state = ref state in
      ignore(Cil.visitCilInstr (new visit_use state stmt :> Cil.cilVisitor) i);
      begin match i with
        | Set (_, e, _)
        | Local_init (_, AssignInit (SingleInit e), _)
          when isConstant e && B.bounded ->
          Some !state
        | Set ((Var vi, offset), _, _)
        | Call (Some (Var vi, offset),_,_,_) ->
          if should_instrument (vi,offset) then
            do_def vi offset stmt !state
          else Some !state
        | Local_init (vi, _, _) ->
          if should_instrument (vi,NoOffset) then
            do_def ~local:true vi NoOffset stmt !state
          else Some !state
        | Call _ | Set _ | Asm _ | Cil_types.Skip _ | Code_annot _ ->
          Some !state
      end
    | Instr _ -> Some state

end

(** Dataflow analysis, Part 3-,4-,5- of the heading comment *)
let do_function ~annot_bound kf mk_label to_add_fun =
  let module DataflowAnalysis = Interpreted_automata.ForwardAnalysis (
      Dataflow (struct let bounded = annot_bound end)
    ) in
  let args = Kernel_function.get_formals kf in
  let f acc arg =
    let defId = get_next_def_id (arg,NoOffset) in
    Def.Set.add (defId, (arg,NoOffset), Kglobal) acc
  in
  let init_d = List.fold_left f Def.Set.empty args in
  let init_state = NonBottom (init_d, Use.Set.empty) in
  let result = DataflowAnalysis.fixpoint kf init_state in
  let visit_stmt stmt state =
    match state with
    | Bottom -> ()
    | NonBottom t ->
      begin
        match stmt.skind with
        | Instr i when not (Utils.is_label i) ->
          ignore(Cil.visitCilStmt (new visit_defuse ~annot_bound t stmt kf mk_label to_add_fun :> Cil.cilVisitor) stmt);
        | Return (Some e,_)
        | If (e,_,_,_)
        | Switch (e,_,_,_) ->
          ignore(Cil.visitCilExpr (new visit_defuse ~annot_bound t stmt kf mk_label to_add_fun :> Cil.cilVisitor) e);
        | _ -> ()
      end
  in
  DataflowAnalysis.Result.iter_stmt visit_stmt result

(** Part 1- and 2- of the heading comment *)
class addSequences ~annot_bound mk_label = object(self)
  inherit Visitor.frama_c_inplace
  val to_add_conds = Stmt.Hashtbl.create 17

  (* get all sequences, sort defs and uses by sequence ID, and returns
     a pair of list (before,after) with sequences to add before & after the current statement *)
  method private get_seqs_sorted stmt =
    let defs =
      if Stmt.Hashtbl.mem to_add_defs stmt then
        List.sort compare @@ Stmt.Hashtbl.find to_add_defs stmt
      else []
    in
    let uses =
      if Stmt.Hashtbl.mem to_add_uses stmt then
        List.sort compare @@ Stmt.Hashtbl.find to_add_uses stmt
      else []
    in
    let conds =
      if Stmt.Hashtbl.mem to_add_conds stmt then
        List.sort compare @@ Stmt.Hashtbl.find to_add_conds stmt
      else []
    in
    (List.map (fun (_,s) -> s) uses) @ (List.map (fun (_,s) -> Stmt_builder.mk s.skind) conds),
    List.map (fun (_,s) -> s) defs

  (** Part 1- and 2- of the heading comment *)
  method! vfunc dec =
    let kf = Option.get self#current_kf in
    if Kernel_function.is_definition kf
    && Annotators.shouldInstrumentFun dec.svar then begin
      let to_add_fun = ref [] in
      do_function ~annot_bound kf mk_label to_add_fun;

      Cil.DoChildrenPost (fun new_dec ->
          let defs = List.sort compare !to_add_fun in
          new_dec.sbody.bstmts <- (List.map (fun (_,s) -> s) defs) @ new_dec.sbody.bstmts;
          Stmt.Hashtbl.reset to_add_conds;
          reset_all ();
          Cfg.clearCFGinfo ~clear_id:false new_dec;
          Cfg.cfgFun new_dec;
          new_dec
        )
    end
    else Cil.SkipChildren

  (** Part 2- a) of the heading comment *)
  method! vblock _ =
    Cil.DoChildrenPost (fun block ->
        let rec aux l acc =
          match l with
          | [] -> acc
          | s :: t ->
            let before,after = self#get_seqs_sorted s in
            s.skind <- Block (Cil.mkBlock (before @ [Stmt_builder.mk s.skind]));
            aux t (acc @ [s] @ after)
        in block.bstmts <- aux block.bstmts [];
        block
      )

  (** Part 2- b) of the heading comment *)
  method! vstmt_aux s =
    if Stmt.Hashtbl.mem to_add_conds_stmt s then
      match s.skind with
      | Instr i when not (Utils.is_label i) ->
        begin match i with
          | Set ((Var _,_),_,_)
          | Call (Some (Var _,_),_,_,_) ->
            let def_set = Stmt.Hashtbl.find to_add_conds_stmt s in
            Def.Set.iter (fun def ->
                if Def.Hashtbl.mem def_to_conds def then
                  Def.Hashtbl.find def_to_conds def
                  |> Stmt_lst.add_list to_add_conds s
              ) def_set;
            Cil.DoChildren
          | _ -> assert false
        end
      | _ -> assert false
    else
      match s.skind with
      | UnspecifiedSequence v ->
        s.skind <- Block (Cil.block_from_unspecified_sequence v); Cil.DoChildren
      | _ -> Cil.DoChildren

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
    Def.Hashtbl.fold (fun _ seqs str ->
        let seqs = List.sort compare seqs in
        List.fold_left (fun acc s -> acc ^ Annotators.next_hl() ^ ") <l" ^ string_of_int s ^"|; ;>,\n") str seqs
      ) hyperlabels ""
  | _ ->
    let symb = operator_of_criterion crit in
    Def.Hashtbl.fold (fun _ seqs str ->
        let seqs = List.sort compare seqs in
        str ^ Annotators.next_hl() ^ ") <" ^ (String.concat symb (List.map (fun s -> "l" ^ string_of_int s) seqs)) ^ "|; ;>,\n"
      ) hyperlabels ""

(** Generate/Append in .hyperlabels file our hyperlabels data *)
let gen_hyperlabels crit =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl crit in
  let out = open_out_gen [Open_creat; Open_append] 0o644 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of hyperlabels = %d" (Annotators.getCurrentHLId())

let visited = ref (false,false)

let is_visited crit =
  match crit, !visited with
  | (ADC | AUC | DUC), (v, _) -> v
  | (BADC | BAUC | BDUC), (_, v) -> v

let set_visited crit =
  match crit, !visited with
  | (ADC | AUC | DUC),    (_, old) -> visited := (true,old)
  | (BADC | BAUC | BDUC), (old, _) -> visited := (old,true)

(** Visit the file with our main visitor addSequences, and mark the new AST as
    changed *)
let visite ?(annot_bound=false) crit file mk_label : unit =
  if not (is_visited crit) then begin
    Visitor.visitFramacFileSameGlobals
      (new addSequences ~annot_bound mk_label :> Visitor.frama_c_visitor) file;
    set_visited crit
  end
  else
    Options.feedback "Avoid annotating with 2 dataflow criteria at the same time"

(** All-defs annotator *)
module ADC = Annotators.Register (struct
    let name = "ADC"
    let help = "All-Definitions Coverage"
    let apply mk_label file =
      visite ADC file mk_label;
      gen_hyperlabels ADC
  end)

(** All-uses annotator *)
module AUC = Annotators.Register (struct
    let name = "AUC"
    let help = "All-Uses Coverage"
    let apply mk_label file =
      visite AUC file mk_label;
      gen_hyperlabels AUC
  end)

(** Def-Use annotator *)
module DUC = Annotators.Register (struct
    let name = "DUC"
    let help = "Definition-Use Coverage"
    let apply mk_label file =
      visite DUC file mk_label;
      gen_hyperlabels DUC
  end)

(** Boundary All-defs annotator *)
module BADC = Annotators.Register (struct
    let name = "BADC"
    let help = "Boundary All-Definitions Coverage"
    let apply mk_label file =
      visite ~annot_bound:true BADC file mk_label;
      gen_hyperlabels BADC
  end)

(** Boundary All-uses annotator *)
module BAUC = Annotators.Register (struct
    let name = "BAUC"
    let help = "Boundary All-Uses Coverage"
    let apply mk_label file =
      visite ~annot_bound:true BAUC file mk_label;
      gen_hyperlabels BAUC
  end)

(** Boundary Def-Use annotator *)
module BDUC = Annotators.Register (struct
    let name = "BDUC"
    let help = "Boundary Definition-Use Coverage"
    let apply mk_label file =
      visite ~annot_bound:true BDUC file mk_label;
      gen_hyperlabels BDUC
  end)