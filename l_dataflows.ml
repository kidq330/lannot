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

module VarOfst =
  Datatype.Pair_with_collections
    (Varinfo)
    (Offset)
    (struct let module_name = "L_dataflow.VarOfst" end)

module IntPair =
  Datatype.Pair_with_collections
    (Datatype.Int)
    (Datatype.Int)
    (struct let module_name = "L_dataflow.IntPair" end)

module Def =
  Datatype.Quadruple_with_collections
    (Datatype.Int) (* Unique ID for each definition *)
    (Datatype.Int) (* Occurence ID for *)
    (VarOfst)      (* Store varinfo and offset of this definition  *)
    (Kinstr)       (* Definition's statement option *)
    (struct let module_name = "L_dataflow.Def" end)

module Use =
  Datatype.Pair_with_collections
    (VarOfst)      (* Store varinfo and offset of this use  *)
    (Stmt)         (* Use's statement option *)
    (struct let module_name = "L_dataflow.Use" end)

module Listtbl (T : Hashtbl.S) = struct
  include T
  let add_list_front tbl key elt =
    if T.mem tbl key then
      let old = T.find tbl key in
      T.replace tbl key (elt::old)
    else
      T.add tbl key [elt]
end
module VarOfst_lst = Listtbl(VarOfst.Hashtbl)
module IntPair_lst = Listtbl(IntPair.Hashtbl)
module Stmt_lst = Listtbl(Stmt.Hashtbl)

(* Represent states in our analysis*)
type t =
  | Bottom
  | NonBottom of (Def.Set.t * Use.Set.t)

(* Count the number of sequences. *)
let nb_seqs = ref 0

(* For each variable, keep the number of assignment seen for this variable
   key : VarOfst.t
   value : def counter
*)
let def_id = VarOfst.Hashtbl.create 32

(* each def is registered to avoid creating them more than once
   key : statement
   value : Def.t
*)
let seen_def = Stmt.Hashtbl.create 32

(* bind each new sequence (def) to its corresponding statement, sequence id is
   used to sort them at the end
   key : statement
   value : (sequence id * statement) list
*)
let to_add_defs = Stmt.Hashtbl.create 32
(* bind each new sequence (use) to its corresponding statement *)
let to_add_uses : (int* stmt) list Stmt.Hashtbl.t = Stmt.Hashtbl.create 32
(* when creating vInfo for sequences' status' variables , bind them to the
   corresponding VarOfst.t *)
let varofst_to_vinfos = VarOfst.Hashtbl.create 32

(* Assign an unique ID to each VarOfst.t *)
let varofst_to_id = VarOfst.Hashtbl.create 32

(* Used to create hyperlabels
   key : variable id * definition id
   value : (sequence id) list
*)
let hyperlabels = IntPair.Hashtbl.create 32

let print_def (id,defId,(vi,ofst),stmt) =
  let stmt =
    match stmt with
    | Kglobal -> -1
    | Kstmt stmt -> stmt.sid
  in
  Format.printf "id : %d / defId: %d / var : %a / stmtDef: %d\n%!"
    id defId Cil_printer.pp_lval (Var vi, ofst) stmt

let print_use ((vi,ofst),stmt) =
  Format.printf "var: %a / sid: %d\n%!"
    Cil_printer.pp_lval (Var vi, ofst) stmt.sid

let print_uses set =
  Printf.printf "Uses : @.";
  Use.Set.iter (fun elt -> Printf.printf "    @."; print_use elt) set

let print_defs set =
  Printf.printf "Defs : @.";
  Def.Set.iter (fun elt -> Printf.printf "    @."; print_def elt) set

let pretty _ = function
  | Bottom -> Printf.printf "Bottom\n@."
  | NonBottom (defs,uses) ->
    Printf.printf "NonBottom : \n@.";
    print_defs defs;
    print_uses uses

(* Clear all hashtbl after each function in addSequences *)
let reset_all () =
  Stmt.Hashtbl.reset to_add_defs;
  Stmt.Hashtbl.reset to_add_uses;
  Stmt.Hashtbl.reset seen_def;
  VarOfst.Hashtbl.reset varofst_to_vinfos;
  VarOfst.Hashtbl.reset def_id

(* Given a variable id, increment the number of defs seen
   and returns it. If it is the first return 1. *)
let get_next_def_id varofst =
  if VarOfst.Hashtbl.mem def_id varofst then
    let n = (VarOfst.Hashtbl.find def_id varofst) + 1 in
    VarOfst.Hashtbl.replace def_id varofst n;
    n
  else
    (VarOfst.Hashtbl.add def_id varofst 1; 1)

let make_def =
  let cpt = ref 0 in
  fun defId var stmt ->
    incr cpt;
    !cpt,defId,var,stmt

let rec trunc_fields offset =
  match offset with
  | Field (f_info,offset') ->
    let offset' = trunc_fields offset' in
    Field (f_info,offset')
  | Index _ | NoOffset -> NoOffset

let get_id_of_varofset =
  let id = ref 0 in
  fun varofst ->
    if VarOfst.Hashtbl.mem varofst_to_id varofst then
      VarOfst.Hashtbl.find varofst_to_id varofst
    else begin
      incr id;
      VarOfst.Hashtbl.add varofst_to_id varofst !id;
      !id
    end

(* Try to find a use in state wich dom/post-dom current use  *)
let is_equivalent varofst stmt kf uses =
  Use.Set.exists (fun (var,stmtUse) ->
      let eq = VarOfst.equal var varofst in
      if eq && not (Dominators.dominates stmtUse stmt) then
        Options.fatal "Discrepancy: This should not happen" varofst;
      eq &&
      !Db.Postdominators.is_postdominator kf ~opening:stmtUse ~closing:stmt
    ) uses

(* Annot use in expression only once  *)
let is_triv_equiv varofst visited =
  Options.CleanEquiv.get() &&
  List.exists (fun varofst' -> VarOfst.equal varofst' varofst) visited

(* Test if a LVal should be instrumented, i.e. it's not a global,
   a temp variable or if it's the first time we see it in the current
   expr/instr (except if CleanDuplicate is true) *)
let should_instrument (v,offset) visited =
  not v.vglob && not (Datatype.String.equal v.vname "__retres") && not v.vtemp
  && Annotators.shouldInstrumentVar v && not (is_triv_equiv (v,offset) visited)

let unk_loc = Location.unknown

(***********************************)
(********* Defuse Criteria *********)
(***********************************)

(* This visitor visits each LVal to create sequences if the state t contains
   defs for theses LVals *)
class visit_defuse (defs_set,uses_set) current_stmt kf mk_label to_add_fun = object(self)
  inherit Visitor.frama_c_inplace
  val mutable visited = []

  method private zero () = Cil.zero unk_loc
  method private one () = Cil.one unk_loc

  method private init_vinfo name =
    Cil.makeVarinfo false false name (TInt(IInt,[]))

  method private mk_set ?(loc=unk_loc) ?(offset=NoOffset) vi value =
    let set = Ast_info.mkassign (Var vi, offset) value loc in
    Cil.mkStmtOneInstr ~valid_sid:true set

  method private mk_comp ?(loc=unk_loc) ?(offset=NoOffset) vi value =
    let new_exp = Cil.new_exp loc (Lval (Var vi, offset)) in
    Ast_const.Exp.binop Cil_types.Eq new_exp value

  method private mk_init ?(loc=unk_loc) vi value =
    let set = Local_init(vi,AssignInit(SingleInit(value)),loc) in
    Cil.mkStmtOneInstr ~valid_sid:true set

  method private register_seq (_,defId,varData,stmtDef) ids vInfo cond suse =
    VarOfst_lst.add_list_front varofst_to_vinfos varData (ids,cond);
    begin match stmtDef with
      | Kglobal ->
        to_add_fun := (ids,self#mk_init vInfo (self#one ())) :: !to_add_fun
      | Kstmt stmt ->
        to_add_fun := (ids,self#mk_init vInfo (self#zero ())) :: !to_add_fun;
        Stmt_lst.add_list_front to_add_defs stmt (ids,self#mk_set vInfo (self#one ()))
    end;
    (* Add the use *)
    Stmt_lst.add_list_front to_add_uses current_stmt (ids,suse);
    (* Register this sequence to its hyperlabel *)
    let id = get_id_of_varofset varData in
    IntPair_lst.add_list_front hyperlabels (id, defId) ids;
    incr nb_seqs

  (* Create a def-use sequence for the given def *)
  method private mkSeq_aux loc def bound =
    let _,_,(vi,offset),stmtDef = def in
    let ids = Annotators.getCurrentLabelId () + 1 in (* sequence id *)
    let vInfo = self#init_vinfo ("__SEQ_STATUS_" ^ string_of_int ids) in
    let cond = self#mk_set vInfo (self#zero ()) in
    let use = self#mk_comp vInfo (self#one ()) in
    let suse = match bound with
      | None -> mk_label use [] loc
      | Some (bound,kind) ->
        let pred_vInfo = self#init_vinfo ("__SEQ_BOUND_" ^ string_of_int ids) in
        let lval_vInfo = Cil.new_exp unk_loc (Lval (Var pred_vInfo,NoOffset)) in
        let bound = Cil.kinteger64 ~loc:unk_loc ~kind bound in
        let pred = self#mk_comp vi ~offset bound in
        begin match stmtDef with
          | Kglobal ->
            to_add_fun := (ids,self#mk_init pred_vInfo pred) :: !to_add_fun
          | Kstmt stmt ->
            to_add_fun := (ids,self#mk_init pred_vInfo (self#zero ())) :: !to_add_fun;
            Stmt_lst.add_list_front to_add_defs stmt (ids,self#mk_set pred_vInfo pred)
        end;
        let use = Ast_const.Exp.binop Cil_types.LAnd use lval_vInfo in
        mk_label use [] loc
    in
    self#register_seq def ids vInfo cond suse

  (* Create a def-use sequence for the given def *)
  method private mkSeq loc def =
    let _,_,(vi,offset),_ = def in
    match Cil.typeOfLval (Var vi, offset) with
    | TInt (kind, _) ->
      if Options.BoundedDataflow.get () then
        let bounds = Utils.get_limits kind in
        List.iter (fun b -> self#mkSeq_aux loc def (Some(b,kind))) bounds
      else self#mkSeq_aux loc def None
    | _ -> self#mkSeq_aux loc def None

  (* Visit all expressions and sub expressions to find lvals *)
  method! vexpr expr =
    match expr.enode with
    | Lval (Var v, offset) ->
      let varofst = (v,trunc_fields offset) in
      if should_instrument varofst visited then begin
        visited <- varofst :: visited;
        (* Keeps defs related to this variable *)
        let all_varofst_defs = Def.Set.filter (fun (_,_,varData,_) -> VarOfst.equal varData varofst) defs_set in
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
class visit_use state stmt = object(_)
  inherit Visitor.frama_c_inplace
  val mutable visited = []

  (* Visit all expressions and sub expressions to find lvals *)
  method! vexpr expr =
    match !state with
    | Bottom -> Cil.SkipChildren
    | NonBottom (defs,uses) ->
      match expr.enode with
      | Lval (Var v, offset) ->
        let varofst = (v,trunc_fields offset) in
        if should_instrument varofst visited then begin
          visited <- varofst :: visited;
          let new_uses = Use.Set.add (varofst,stmt) uses in
          state := NonBottom (defs, new_uses);
        end;
        Cil.DoChildren
      | _ -> Cil.DoChildren
end

module Inst = struct

  let pretty = pretty

  (* Return a new set after removing all definitions of a variable *)
  let remove_def varofst s =
    Def.Set.filter (fun (_,_,varData,_) -> not (VarOfst.equal varData varofst)) s

  (* Return a new set after removing all uses of a variable *)
  let remove_use varofst s =
    Use.Set.filter (fun (var,_) -> not (VarOfst.equal var varofst)) s

  type nonrec t = t

  (* Function called to join 2 states *)
  let join a b =
    match a,b with
    | Bottom, x | x, Bottom -> x
    | NonBottom (d,u), NonBottom (d',u') ->
      NonBottom (Def.Set.union d d',Use.Set.inter u u')

  (* is the set a is a subset of b *)
  let is_included a b =
    match a,b with
    | Bottom, _ -> true
    | NonBottom _, Bottom -> false
    | NonBottom (d,u), NonBottom (d',u') ->
      Def.Set.subset d d' && Use.Set.subset u u'

  let join_and_is_included a b =
    join a b, is_included a b

  let bottom = Bottom

  (* For each definition statement, update the current state by
     adding or not new definition, removing older ones etc... *)
  let do_def ?(local=false) v offset stmt = function
    | Bottom -> Bottom
    | NonBottom (defs,uses) ->
      let varofst = (v,trunc_fields offset) in
      let defs_clean =
        if local || Options.CleanDataflow.get () then
          remove_def varofst defs
        else
          defs
      in
      let uses_clean = remove_use varofst uses in
      let new_d =
        if Stmt.Hashtbl.mem seen_def stmt then
          Def.Set.add (Stmt.Hashtbl.find seen_def stmt) defs_clean
        else begin
          let defId = get_next_def_id varofst in
          let new_def = make_def defId varofst (Kstmt stmt) in
          Stmt.Hashtbl.add seen_def stmt new_def;
          Def.Set.add new_def defs_clean
        end
      in
      NonBottom (new_d,uses_clean)

  (* Function called for each stmt and propagating new states to each succs of stmt *)
  let transfer_stmt stmt state =
    match stmt.skind with
    | Instr i when not (Utils.is_label i) ->
      let state = ref state in
      ignore(Cil.visitCilInstr (new visit_use state stmt :> Cil.cilVisitor) i);
      begin match i with
        | Set ((Var v,offset),_,_)
        | Call (Some (Var v,offset),_,_,_) ->
          if not (Datatype.String.equal v.vname "__retres") && not v.vtemp then begin
            let res = do_def v offset stmt !state in
            List.map (fun x -> (x,res)) stmt.succs
          end
          else List.map (fun x -> (x,!state)) stmt.succs
        | Local_init (v,_,_) ->
          if not (Datatype.String.equal v.vname "__retres") && not v.vtemp then begin
            let res = do_def ~local:true v NoOffset stmt !state in
            List.map (fun x -> (x,res)) stmt.succs
          end
          else List.map (fun x -> (x,!state)) stmt.succs
        | _ ->
          List.map (fun x -> (x,!state)) stmt.succs
      end
    | Return (Some e,_)
    | If (e,_,_,_)
    | Switch (e,_,_,_) ->
      let state = ref state in
      ignore(Cil.visitCilExpr  (new visit_use state stmt :> Cil.cilVisitor) e);
      List.map (fun x -> (x,!state)) stmt.succs
    | _ -> List.map (fun x -> (x,state)) stmt.succs

end

(* For each function, do a dataflow analysis and create sequences
   Initial state contains def of each formal
*)
let do_function kf mk_label to_add_fun =
  let module Fenv = (val Dataflows.function_env kf) in
  let args = Kernel_function.get_formals kf in
  let first_stmt = Kernel_function.find_first_stmt kf in
  let f acc arg =
    let defId = get_next_def_id (arg,NoOffset) in
    Def.Set.add (make_def defId (arg,NoOffset) Kglobal) acc
  in
  let init_d = List.fold_left f Def.Set.empty args in
  let module Arg = struct
    include Inst
    let init =
      [(first_stmt, NonBottom (init_d, Use.Set.empty))]

  end in
  let module Results = Dataflows.Simple_forward(Fenv)(Arg) in
  let f stmt state =
    match state with
    | Bottom -> ()
    | NonBottom t ->
      begin
        match stmt.skind with
        | Instr i when not (Utils.is_label i) ->
          ignore(Cil.visitCilInstr (new visit_defuse t stmt kf mk_label to_add_fun :> Cil.cilVisitor) i);
        | Return (Some e,_)
        | If (e,_,_,_)
        | Switch (e,_,_,_) ->
          ignore(Cil.visitCilExpr  (new visit_defuse t stmt kf mk_label to_add_fun :> Cil.cilVisitor) e);
        | _ -> ()
      end
  in
  Results.iter_on_result f

(* This visitor will, for each function :
   - Do the dataflows analysis and fill hashtbls
   - Then visit the body of the function, add add all sequences to where
     they belong.
   - Clean all hashtbls and do the next function (if any)
*)
class addSequences mk_label = object(self)
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
    List.map (fun (_,s) -> s) defs, (List.map (fun (_,s) -> s) uses) @ (List.map (fun (_,s) -> s) conds)

  method! vfunc (dec : fundec) : fundec Cil.visitAction =
    let kf = Option.get self#current_kf in
    if Kernel_function.is_definition kf && Annotators.shouldInstrumentFun dec.svar then begin
      let to_add_fun = ref [] in
      Cfg.clearCFGinfo ~clear_id:false dec;
      Cfg.cfgFun dec;
      do_function kf mk_label to_add_fun;

      Cil.DoChildrenPost (fun f ->
          let defs = List.sort compare !to_add_fun in
          f.sbody.bstmts <- (List.map (fun (_,s) -> s) defs) @ f.sbody.bstmts;
          reset_all ();
          f
        )
    end
    else Cil.SkipChildren

  method! vblock _ =
    Cil.DoChildrenPost (fun block ->
        let rec aux l acc =
          match l with
          | [] -> acc
          | s :: t ->
            let after,before = self#get_seqs_sorted s in
            (* if the statement has 1 or more labels, then moves it to
               the first statement of before if it exists *)

            if s.labels <> [] && before <> [] then begin
              s.skind <- Block (Ast_const.Block.mk (before @ [Ast_const.Stmt.mk s.skind]));
              aux t (acc @ [s] @ after)
            end
            else
              aux t (acc @ before @ [s] @ after)
        in block.bstmts <- aux block.bstmts [];
        block
      )

  method! vstmt s =
    match s.skind with
    | Instr i when not (Utils.is_label i) ->
      begin match i with
        | Set ((Var v,offset),_,_)
        | Call (Some (Var v,offset),_,_,_) ->
          let varofst = (v, trunc_fields offset) in
          if VarOfst.Hashtbl.mem varofst_to_vinfos varofst then
            Stmt.Hashtbl.add to_add_conds s (VarOfst.Hashtbl.find varofst_to_vinfos varofst);
          Cil.DoChildren
        | _ -> Cil.DoChildren
      end
    | UnspecifiedSequence v ->
      s.skind <- Block (Cil.block_from_unspecified_sequence v); Cil.DoChildren
    | _ -> Cil.DoChildren

end

type criterion = ADC | AUC | DUC

let string_of_criterion = function
  | ADC -> "+"
  | AUC -> "."
  | DUC -> ""

(** Create all hyperlabels *)
let compute_hl crit =
  match crit with
  | DUC ->
    IntPair.Hashtbl.fold (fun _ seqs str ->
        let seqs = List.sort compare seqs in
        List.fold_left (fun acc s -> acc ^ Annotators.next_hl() ^ ") <l" ^ string_of_int s ^"|; ;>,\n") str seqs
      ) hyperlabels ""
  | ADC | AUC ->
    let symb = string_of_criterion crit in
    IntPair.Hashtbl.fold (fun _ seqs str ->
        let seqs = List.sort compare seqs in
        str ^ Annotators.next_hl() ^ ") <" ^ (String.concat symb (List.map (fun s -> "l" ^ string_of_int s) seqs)) ^ "|; ;>,\n"
      ) hyperlabels ""

let gen_hyperlabels crit =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl crit in
  let out = open_out_gen [Open_creat; Open_append] 0o644 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of sequences = %d" !nb_seqs;
  Options.feedback "Total number of hyperlabels = %d" (Annotators.getCurrentHLId())

(** Successively pass the 2 visitors *)
let visite (file : file) mk_label : unit =
  Visitor.visitFramacFileSameGlobals (new addSequences mk_label :> Visitor.frama_c_visitor) file;
  Ast.mark_as_changed ()

(** All-defs annotator *)
module ADC = Annotators.Register (struct
    let name = "ADC"
    let help = "All-Definitions Coverage"
    let apply mk_label file =
      visite file mk_label;
      gen_hyperlabels ADC
  end)

(** All-uses annotator *)
module AUC = Annotators.Register (struct
    let name = "AUC"
    let help = "All-Uses Coverage"
    let apply mk_label file =
      visite file mk_label;
      gen_hyperlabels AUC
  end)

(** Def-Use annotator *)
module DUC = Annotators.Register (struct
    let name = "DUC"
    let help = "Definition-Use Coverage"
    let apply mk_label file =
      visite file mk_label;
      gen_hyperlabels DUC
  end)
