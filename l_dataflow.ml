(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  You may redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.                                                *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful, but WITHOUT     *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General      *)
(*  Public License for more details.                                      *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3 for more          *)
(*  details (enclosed in the file LICENSE).                               *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Ast_const

(** Number of defs for each variable *)
let nBVarDefs : (int,int) Hashtbl.t = Hashtbl.create 32
(** Number of uses for each variable *)
let nBVarUses : (int,int) Hashtbl.t = Hashtbl.create 32

(** Current def for each variable *)
let currentDef : (int,int) Hashtbl.t = Hashtbl.create 32
(** Current use for each variable *)
let currentUse : (int,int) Hashtbl.t = Hashtbl.create 32

(** Store for each couple (variable id, loop id) a unique id *)
let varLoopID : (int*int,int) Hashtbl.t  = Hashtbl.create 32
(** Using varCounter, associate to each nuplet (Variable id, Def id, Use id)
    a unique (for this variable) id *)
let varDefUseID  : (int*int*int,int) Hashtbl.t = Hashtbl.create 32

(** Map each pc_label_sequence to its stmt *)
let seq_to_stmt = Hashtbl.create 32

let index_vid : (int*int,int) Hashtbl.t = Hashtbl.create 32
let vid_to_index : (int,int) Hashtbl.t = Hashtbl.create 32

(** Store use labels *)
let labelUses : (stmt list) ref = ref []
(** Store def labels *)
let labelDefs : (stmt list) ref = ref []
(** Store conditions labels *)
let labelStops : (stmt list) ref = ref []
(** Store a nuplet (Variable id, Def id, Sequence Id).
    Will be used to create hyperlabels *)
let idList : ((int*int*int) list) ref = ref []

(** Hyperlabel's type *)
let symb : string ref = ref ""

(** Associates a unique id to a couple (Variable id, loop id) and store/return it (or return it if already exists *)
let get_varLoop_id (vid : int) (lid:int) : int =
  if Hashtbl.mem varLoopID (vid,lid) then
    Hashtbl.find varLoopID (vid,lid)
  else begin
    let new_id = Cil_const.new_raw_id () in
    Hashtbl.add varLoopID (vid,lid) new_id;
    new_id
  end

(** Associates an id to a nuplet (Variable id, Def id, Use id), store it and return it. If already exist return and remove it from the hashtbl *)
let get_seq_id (vid : int) (def : int) (use : int) : int =
  if Hashtbl.mem varDefUseID (vid,def,use) then
    let tmp = Hashtbl.find varDefUseID (vid,def,use) in
    Hashtbl.remove varDefUseID (vid,def,use);
    tmp
  else begin
    let new_id = Annotators.next() in
    Hashtbl.add varDefUseID (vid,def,use) new_id;
    new_id
  end

let get_all_index_vid (vid:int) : int list =
  let all_index = Hashtbl.find_all vid_to_index vid in
  List.map (fun i -> Hashtbl.find index_vid (vid,i)) all_index

let get_index_vid vid index =
  if Hashtbl.mem index_vid (vid,index) then
    Hashtbl.find index_vid (vid,index)
  else begin
    let new_vid =  Cil_const.new_raw_id () in
    Hashtbl.add index_vid (vid,index) new_vid;
    Hashtbl.add vid_to_index vid index;
    new_vid
  end

let extract_index vid index =
  if not (Options.FoldIndex.get()) then [vid]
  else
    match index with
    | Index(index,NoOffset) ->
      begin match Cil.constFoldToInt index with
        | None ->
          vid :: get_all_index_vid vid
        | Some i ->
          let i = Integer.to_int i in
          [get_index_vid vid i]
      end
    | _ ->
      [vid]

(** Visitor that will count the number of Def/Use for each variable *)
class countDefUse = object(self)
  inherit Visitor.frama_c_inplace

  (** Stack that store loops id wen entering in it, and pop it when leaving *)
  val inLoopId : int Stack.t = Stack.create ()
  val visited = Hashtbl.create 32
  (** Take 2 hashtbl (nbVarDefs/Uses, currentDef/Use) and create/replace the binding of this Id *)
  method private fill_aux (nBVar : (int, int) Hashtbl.t) (current : (int, int) Hashtbl.t) (vid : int) : unit =
    if (Hashtbl.mem nBVar vid) then
      (Hashtbl.replace nBVar vid ((Hashtbl.find nBVar vid) + 1))
    else
      (Hashtbl.add nBVar vid 1; Hashtbl.add current vid 1)

  (** Call fill_aux for the "normal" vid, and call it again if we're in a loop with the
      id assiociated to this variable id and loop id *)
  method private fill_tbl (nBVar : (int, int) Hashtbl.t) (current : (int, int) Hashtbl.t) (vid : int) : unit =
    self#fill_aux nBVar current vid;

    if not (Stack.is_empty inLoopId) then begin
      let lid = Stack.top inLoopId in
      let nvid = get_varLoop_id vid lid in
      self#fill_aux nBVar current nvid
    end

  method! vfunc (dec : Cil_types.fundec) : Cil_types.fundec Cil.visitAction =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      let parList = dec.sformals in
      List.iter (fun v -> self#fill_tbl nBVarDefs currentDef v.vid ) parList;
      Cil.DoChildren
    end

  method private should_instrument v vid =
    if not v.vglob && not (v.vname = "__retres") && not v.vtemp then
      let sid = (Extlib.the self#current_stmt).sid in
      if (not (Options.CleanDuplicate.get()) || not (Hashtbl.mem visited (sid,vid))) then
        Some(sid)
      else None
    else None

  method! vstmt_aux (stmt : Cil_types.stmt) : Cil_types.stmt Cil.visitAction =
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* Ignore labels *)
    | Instr (Set ((Var v,index),_,_))
    | Instr (Call (Some (Var v,index),_,_,_)) ->
       Cil.DoChildrenPost (fun s ->
          let vids = extract_index v.vid index in
          let f vid =
            if not (v.vname = "__retres") && not v.vtemp then
              self#fill_tbl nBVarDefs currentDef vid
          in
          List.iter f vids;
          s
        )
    | Instr (Local_init (v,_,_)) ->
      Cil.DoChildrenPost (fun s ->
          if not (v.vname = "__retres") && not v.vtemp then
            self#fill_tbl nBVarDefs currentDef v.vid;
          s
        )
    | Loop (_,b,_,_,_) ->
      Stack.push stmt.sid inLoopId;
      ignore(Cil.visitCilBlock (self :> Cil.cilVisitor) b);
      ignore(Stack.pop inLoopId);
      Cil.SkipChildren
    | UnspecifiedSequence v ->
      stmt.skind <- Block (Cil.block_from_unspecified_sequence v); Cil.DoChildren
    | _ -> Cil.DoChildren

  method! vexpr (expr : Cil_types.exp) : Cil_types.exp Cil.visitAction =
    match expr.enode with
    | Lval (Var v,index) ->
      let vids = extract_index v.vid index in
      let f vid =
        match self#should_instrument v vid with
        | None -> ()
        | Some(sid) ->
          Hashtbl.add visited (sid,vid) vid;
          self#fill_tbl nBVarUses currentUse vid
      in
      List.iter f vids;
      if v.vtemp then Cil.SkipChildren else Cil.DoChildren
    | _ -> Cil.DoChildren
end

(** Visitor that will add all labels using the previously filled Hashtbl **)
class addLabels = object(self)
  inherit Visitor.frama_c_inplace

  (** Stack that store loops id wen entering in it, and pop it when leaving *)
  val inLoopId = Stack.create ()
  val visited = Hashtbl.create 32
  (** Create a pc_label_sequence and store it in the corresponding list of labels *)
  method private mkSeq (ids : int) (vid : int) (nb : int) : unit =
    let idExp = Exp.kinteger IULong ids in
    let oneExp = Exp.one () in
    let curr = Exp.integer nb in
    let slen = Exp.integer 2 in
    let varExp = Exp.string (string_of_int vid) in
    let zeroExp = Exp.zero () in
    let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;curr;slen;varExp;zeroExp])) in
    Hashtbl.add seq_to_stmt ids newStmt.sid;
    if nb = 1 (*Def*) then
      labelDefs := !labelDefs@[newStmt]
    else (* Use *)
      labelUses := !labelUses@[newStmt]

  (** Create a pc_label_sequence_condiion and store it in the corresponding list of labels *)
  method private mkCond (vid : int) : unit =
    let zeroExp = Exp.zero () in
    let ccExp = Exp.string (string_of_int vid) in
    let newStmt = (Utils.mk_call "pc_label_sequence_condition" ([zeroExp;ccExp])) in
    labelStops := newStmt :: !labelStops

  (** Called for each parameters of a function, and the corresponding sequences *)
  method private handle_param (v : Cil_types.varinfo) : unit  =
    if Hashtbl.mem nBVarUses v.vid then begin
      for j = 1 to (Hashtbl.find nBVarUses v.vid) do
        let ids = get_seq_id v.vid 1 j in
        idList := (v.vid,1,ids) :: !idList;
        self#mkSeq ids v.vid 1
      done;
      Hashtbl.replace currentDef v.vid 2
    end

  (** Take a variable that is currenlty being defined, and add the corresponding sequences *)
  method private process_def ?(index=NoOffset) (v : Cil_types.varinfo) : unit  =
    let vids = extract_index v.vid index in
    let f vid =
      if not (v.vname = "__retres") && not v.vtemp && Hashtbl.mem nBVarUses vid then begin
        self#mkCond vid;
        let defId = (Hashtbl.find currentDef vid) in
        (* For each following uses *)
        for j = (Hashtbl.find currentUse vid) to (Hashtbl.find nBVarUses vid) do
          let ids = get_seq_id vid defId j in
          idList := (vid,defId,ids) :: !idList;
          self#mkSeq ids vid 1
        done;
        (Hashtbl.replace currentDef vid (defId + 1));

        if not (Stack.is_empty inLoopId) then begin
          let lid = Stack.top inLoopId in
          let nvid = get_varLoop_id vid lid in
          if Hashtbl.mem nBVarUses nvid then begin
            let i = (Hashtbl.find currentDef nvid) in
            (* For each preceding uses *)
            for j = 1 to (Hashtbl.find currentUse nvid) - 1 do
              let ids = get_seq_id nvid i j in
              idList := (vid,defId,ids) :: !idList;
              self#mkSeq ids vid 1;
              Hashtbl.remove varDefUseID (nvid,i,j)
            done;
            Hashtbl.replace currentDef nvid (i + 1)
          end
        end
      end
    in
    List.iter f vids

  method! vfunc (dec : Cil_types.fundec) : Cil_types.fundec Cil.visitAction =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      let parList = dec.sformals in
      List.iter self#handle_param parList;
      let labelParams = !labelDefs in
      labelDefs := [];
      Cil.DoChildrenPost (fun dec -> dec.sbody.bstmts <- labelParams @ dec.sbody.bstmts; dec)
    end

  method! vstmt_aux (stmt : Cil_types.stmt) : Cil_types.stmt Cil.visitAction =
    let lbl = List.length stmt.labels <> 0 in
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* ignorer les labels *)
    | Instr (Set ((Var v,index),_,_))
    | Instr (Call (Some (Var v,index),_,_,_)) ->
      Cil.DoChildrenPost (fun stmt ->
          self#process_def ~index:index v;
          (* if this statement is associated to one or more C labels, then sequences will be placed
             between this labels and the statement itself *)
          let res =
            if not lbl then
              Stmt.block (!labelUses @ !labelStops @ [stmt] @ !labelDefs)
            else
              Stmt.block ({stmt with skind = Block (Block.mk (!labelUses @ !labelStops @ [Stmt.mk stmt.skind]))} :: !labelDefs)
          in
          labelUses := [];
          labelDefs := [];
          labelStops := [];
          res
        )
    | Instr (Local_init (v,_,_)) ->
      Cil.DoChildrenPost (fun stmt ->
          self#process_def v;
          (* if this statement is associated to one or more C labels, then sequences will be placed
             between this labels and the statement itself *)
          let res =
            if not lbl then
              Stmt.block (!labelUses @ !labelStops @ [stmt] @ !labelDefs)
            else
              Stmt.block ({stmt with skind = Block (Block.mk (!labelUses @ !labelStops @ [Stmt.mk stmt.skind]))} :: !labelDefs)
          in
          labelUses := [];
          labelDefs := [];
          labelStops := [];
          res
        )
    | If (ex,th,el,lo) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) ex);
      (let lu = !labelUses in labelUses := [];
       let thenb = (Cil.visitCilBlock (self :> Cil.cilVisitor) th) in
       let elseb = (Cil.visitCilBlock (self :> Cil.cilVisitor) el) in
       let newSt = (Block.mk (lu @ [Stmt.mk (If (ex,thenb,elseb,lo))])) in stmt.skind <- (Block newSt);
       Cil.ChangeTo stmt)
    | Switch (ex, b, stmtl, lo) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) ex);
      (let lu = !labelUses in labelUses := [];
       let nb = (Cil.visitCilBlock (self :> Cil.cilVisitor) b) in
       let newSt = (Block.mk (lu @ [Stmt.mk (Switch (ex,nb,stmtl,lo))])) in stmt.skind <- (Block newSt);
       Cil.ChangeTo stmt)
    | Loop (ca,b,l,s1,s2) ->
      Stack.push stmt.sid inLoopId;
      let nb = (Cil.visitCilBlock (self :> Cil.cilVisitor) b) in
      ignore(Stack.pop inLoopId);
      let newst = Loop (ca, nb, l, s1, s2) in
      stmt.skind <- newst;
      Cil.ChangeTo stmt
    | UnspecifiedSequence v ->
      stmt.skind <- Block (Cil.block_from_unspecified_sequence v); Cil.DoChildren
    | _ ->
      Cil.DoChildrenPost (fun stmt ->
          let res =
            if not lbl then
              Stmt.block (!labelUses @ [stmt])
            else begin
              stmt.skind <-Block (Block.mk (!labelUses @ [Stmt.mk stmt.skind]));
              stmt
            end
          in
          labelUses := []; res
        )

  method private should_instrument v vid =
    if not v.vglob && not (v.vname = "__retres") && not v.vtemp && Hashtbl.mem nBVarDefs vid then
      let sid = (Extlib.the self#current_stmt).sid in
      if (not (Options.CleanDuplicate.get()) || not (Hashtbl.mem visited (sid,vid))) then
        Some(sid)
      else None
    else None

  method! vexpr (expr : Cil_types.exp) : Cil_types.exp Cil.visitAction =
    match expr.enode with
    | Lval (Var v, index) ->
      let vids = extract_index v.vid index in
      let f vid =
        match self#should_instrument v vid with
        | None -> ()
        | Some(sid) ->
          Hashtbl.add visited (sid,vid) vid;
          let j = (Hashtbl.find currentUse vid) in
          (* For each preceding Def *)
          for i = 1 to (Hashtbl.find currentDef vid) - 1  do
            let ids = get_seq_id vid i j in
            self#mkSeq ids vid 2;
            Hashtbl.remove varDefUseID (vid,i,j)
          done;
          (Hashtbl.replace currentUse vid (j + 1));

          if not (Stack.is_empty inLoopId) then begin
            let lid = Stack.top inLoopId in
            let nvid = get_varLoop_id vid lid in
            if Hashtbl.mem nBVarDefs nvid then begin
              let j = (Hashtbl.find currentUse nvid) in
              (* For each following Def *)
              for i = (Hashtbl.find currentDef nvid) to (Hashtbl.find nBVarDefs nvid) do
                let ids = get_seq_id nvid i j in
                self#mkSeq ids vid 2
              done;
              Hashtbl.replace currentUse nvid (j + 1)
            end
          end
      in
      List.iter f vids;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

(** Use idList to create all hyprlabels *)
let compute_hl () : string =
  let regroup = Hashtbl.create 128 in
  let fill (vid,defId,ids) =
    if Hashtbl.mem regroup (vid,defId) then
      Hashtbl.replace regroup (vid,defId) (ids :: (Hashtbl.find regroup (vid,defId)))
    else
      Hashtbl.add regroup (vid,defId) [ids]
  in
  List.iter fill !idList;
  if "-" = !symb then
    Hashtbl.fold (fun _ seqs str ->
        List.fold_left (fun acc s -> acc ^ Annotators.next_hl() ^ ") <s" ^ string_of_int s ^"|; ;>,\n") str seqs
      ) regroup ""
  else
    Hashtbl.fold (fun _ seqs str ->
        str ^ Annotators.next_hl() ^ ") <" ^ (String.concat !symb (List.map (fun s -> "s" ^ string_of_int s) seqs)) ^ "|; ;>,\n"
      ) regroup ""

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl () in
  let out = open_out_gen [Open_creat; Open_append] 0o644 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of sequences = %d" (List.length !idList);
  Options.feedback "Total number of hyperlabels = %d" (Annotators.getCurrentHLId())


(** Successively pass the 2 visitors *)
let visite (file : Cil_types.file) : unit =
  Visitor.visitFramacFileSameGlobals (new countDefUse :> Visitor.frama_c_visitor) file;
  Visitor.visitFramacFileSameGlobals (new addLabels :> Visitor.frama_c_visitor) file;
  Cfg.clearFileCFG ~clear_id:false file;
  Cfg.computeFileCFG file;
  Ast.mark_as_changed ();
  if Options.CleanDataflow.get() then begin
    let old_length = List.length !idList in
    Clean_dataflow.clean idList seq_to_stmt file;
    Options.feedback "%d infeasible sequences were removed,\n use -lannot-no-clean to disable this feature" (old_length - (List.length !idList));
  end


(** All-defs annotator *)
module AllDefs = Annotators.Register (struct
    let name = "alldefs"
    let help = "All-Definitions Coverage"
    let apply _ file =
      visite file;
      symb := "+";
      gen_hyperlabels ()
  end)


(** All-uses annotator *)
module AllUses = Annotators.Register (struct
    let name = "alluses"
    let help = "All-Uses Coverage"
    let apply _ file =
      visite file;
      symb := ".";
      gen_hyperlabels ()
  end)

(** Def-Use annotator *)
module Defuse = Annotators.Register (struct
    let name = "defuse"
    let help = "Definition-Use Coverage"
    let apply _ file =
      visite file;
      symb := "-";
      gen_hyperlabels ()
  end)
