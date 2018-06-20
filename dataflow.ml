open Cil_types
open Ast_const

let nBVarDefs = Hashtbl.create 32
let nBVarUses = Hashtbl.create 32

let currentDef = Hashtbl.create 32
let currentUse = Hashtbl.create 32

let varCounter = Hashtbl.create 32
let varLoopID = Hashtbl.create 32
let varDefUseID = Hashtbl.create 32

let labelUses = ref []
let labelDefs = ref []
let labelStops = ref []
let idList = ref []

let symb = ref ""
let temp = ref ""

let () = Options.result "TODO : Check Set/Use\
                         Gérer les fields de struct (easy)"

(*
(* Récupère l'index à partir d'un offset sous forme d'entier quand c'est possible *)
let get_index_from_offset offset =
  match offset with
  | Index (e,_) ->
    let i = Cil.constFoldToInt e in
    (match i with
     | None -> None
     | Some(int) -> Some(Integer.to_int int))
  | _ -> None

(* Récupère l'index à partir d'un skind sous forme d'entier quand c'est possible *)
let get_index_from_skind skind =
  match skind with
  | Instr (Set ((Var _,offset),_,_))
  | Instr (Call (Some (Var _,offset),_,_,_)) ->
    get_index_from_offset offset
  | _ -> None

           (* Attribut un identifiant unique à un couple (vid,index) *)
let get_index_id vid index =
  match index with
  | None -> None
  | Some(i) ->
    if not (Hashtbl.mem pairVarOffset (vid,i)) then begin
      let new_id = Cil_const.new_raw_id () in
      Hashtbl.add pairVarOffset (vid,i) new_id;
      Some(new_id)
    end
    else
      Some(Hashtbl.find pairVarOffset (vid,i))

(* Retourne l'identifiant de variable en prenant en compte un index s'il existe et que l'option est activé *)
let get_rvid_offset vid offset =
  if not (Options.ConstantFoldingArray.get ()) then
    vid
  else
    let index = get_index_from_offset offset in
    match get_index_id vid index with
    | None -> vid
    | Some(i) -> i

let get_rvid_skind vid skind =
  if not (Options.ConstantFoldingArray.get ()) then
    vid
  else
    let index = get_index_from_skind skind in
    match get_index_id vid index with
    | None -> vid
    | Some(i) -> i
*)

(* Attribut un identifiant unique pour une variable dans une boucle *)
let get_varLoop_id vid lid =
  if Hashtbl.mem varLoopID (vid,lid) then
    Hashtbl.find varLoopID (vid,lid)
  else begin
    let new_id = Cil_const.new_raw_id () in
    Hashtbl.add varLoopID (vid,lid) new_id;
    new_id
  end

(*
let varFieldID = Hashtbl.create 32

let get_field_id vid field =
  if Hashtbl.mem varFieldID (vid,field) then
    Hashtbl.find varFieldID (vid,field)
  else begin
    let new_id = Cil_const.new_raw_id () in
    Hashtbl.add varLoopID (vid,field) new_id;
    new_id
  end*)

let cantor_pairing n m = (((n+m)*(n+m+1))/2)+m
(* Attribut un identifiant unique de séquence pour d'un triplet (vid,def,use) à partir d'un compteur lié à vid *)
let get_seq_id vid def use =
  if Hashtbl.mem varDefUseID (vid,def,use) then
    cantor_pairing vid (Hashtbl.find varDefUseID (vid,def,use))
  else begin
    if not (Hashtbl.mem varCounter vid) then
      Hashtbl.add varCounter vid (ref 0);
    let cpt = Hashtbl.find varCounter vid in
    incr cpt;
    Hashtbl.add varDefUseID (vid,def,use) !cpt;
    cantor_pairing vid !cpt
  end

(** Visitor Count **)
class countDefUse = object(self)
  inherit Visitor.frama_c_inplace

  val inLoopId = Stack.create ()

  method fill_aux nBVar current vid =
    if (Hashtbl.mem nBVar vid) then
      (Hashtbl.replace nBVar vid ((Hashtbl.find nBVar vid) + 1))
    else
      (Hashtbl.add nBVar vid 1; Hashtbl.add current vid 1)

  method fill_tbl nBVar current vid =
    self#fill_aux nBVar current vid;
    (*Dans le cas ou le Def/Use se trouve dans une loop, on fait la même chose avec un identifiant lié à la boucle *)
    if not (Stack.is_empty inLoopId) then begin
      let lid = Stack.top inLoopId in
      let nvid = get_varLoop_id vid lid in
      self#fill_aux nBVar current nvid
    end

  (* Def-Param *)
  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      let parList = dec.sformals in
      List.iter (fun v -> self#fill_tbl nBVarDefs currentDef v.vid ) parList;
      Cil.DoChildren
    end

  (* Def-Var *)
  method! vstmt_aux stmt =
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* Ignorer les labels *)
    | Instr (Set ((Var v,_),_,_))
    | Instr (Call (Some (Var v,_),_,_,_))
    | Instr (Local_init (v,_,_)) ->
      Cil.DoChildrenPost (fun f ->
          if not (v.vname = "__retres") && not v.vtemp then
            self#fill_tbl nBVarDefs currentDef v.vid;
          f
        )
    | Loop (_,b,_,_,_) ->
      Stack.push stmt.sid inLoopId;
      ignore(Cil.visitCilBlock (self :> Cil.cilVisitor) b);
      ignore(Stack.pop inLoopId);
      Cil.SkipChildren
    | _ -> Cil.DoChildren

  (* Use *)
  method! vexpr expr =
    match expr.enode with
    | Lval (Var v,offset) ->
      if not v.vglob && not (v.vname = "__retres" ) && not v.vtemp then
        self#fill_tbl nBVarUses currentUse v.vid;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

(** Visitor Add Labels **)
class addLabels = object(self)
  inherit Visitor.frama_c_inplace

  val inLoopId = Stack.create ()

  method mkSeq ids vid nb =
    let idExp = Exp.kinteger IULong ids in
    let oneExp = Exp.one () in
    let curr = Exp.integer nb in
    let slen = Exp.integer 2 in
    let varExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int vid))) in
    let zeroExp = Exp.zero () in
    let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;curr;slen;varExp;zeroExp])) in
    if nb = 1 (*Def*) then
      labelDefs := newStmt :: !labelDefs
    else (* Use *)
      labelUses := newStmt :: !labelUses

  method mkCond vid =
    let zeroExp = Exp.zero () in
    let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int vid))) in
    let newStmt = (Utils.mk_call "pc_label_sequence_condition" ([zeroExp;ccExp])) in
    labelStops := newStmt :: !labelStops

  method process_def v =
    let vid = v.vid in
    if not (v.vname = "__retres") && not v.vtemp && Hashtbl.mem nBVarUses vid then begin
      self#mkCond vid;
      let defId = (Hashtbl.find currentDef vid) in
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
          for j = 1 to (Hashtbl.find currentUse nvid) - 1 do
            let ids = get_seq_id nvid i j in
            idList := (vid,defId,ids) :: !idList;
            self#mkSeq ids vid 1
          done;
          Hashtbl.replace currentDef nvid (i + 1)
        end
      end
    end

  method handle_param v =
    if Hashtbl.mem nBVarUses v.vid then begin
      for j = 1 to (Hashtbl.find nBVarUses v.vid) do
        let ids = get_seq_id v.vid 1 j in
        idList := (v.vid,1,ids) :: !idList;
        self#mkSeq ids v.vid 1
      done;
      Hashtbl.replace currentDef v.vid 2
    end

  (* Def-Param *)
  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      let parList = dec.sformals in
      List.iter self#handle_param parList;
      let labelParams = !labelDefs in
      labelDefs := [];
      Cil.DoChildrenPost (fun dec -> (dec.sbody.bstmts <- (labelParams @ dec.sbody.bstmts));  dec)
    end

  (* Def-Var *)
  method! vstmt_aux stmt =
    let lbl = List.length stmt.labels != 0 in
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* ignorer les labels *)
    | Instr (Set ((Var v,_),_,_))
    | Instr (Call (Some (Var v,_),_,_,_))
    | Instr (Local_init (v,_,_)) ->
      Cil.DoChildrenPost (fun stmt ->
          self#process_def v;
          let res =
            if not lbl then
              Stmt.block (!labelUses @ !labelStops @ [stmt] @ !labelDefs)
            else
              Stmt.block ({stmt with skind = Block (Block.mk (!labelUses @ !labelStops @ [Cil.mkStmt stmt.skind]))} :: !labelDefs)
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
       let newSt = (Block.mk (lu @ [Cil.mkStmt (If (ex,thenb,elseb,lo))])) in stmt.skind <- (Block newSt);
       Cil.ChangeTo stmt)
    | Switch (ex, b, stmtl, lo) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) ex);
      (let lu = !labelUses in labelUses := [];
       let nb = (Cil.visitCilBlock (self :> Cil.cilVisitor) b) in
       let newSt = (Block.mk (lu @ [Cil.mkStmt (Switch (ex,nb,stmtl,lo))])) in stmt.skind <- (Block newSt);
       Cil.ChangeTo stmt)
    | Loop (ca,b,l,s1,s2) ->
      Stack.push stmt.sid inLoopId;
      let nb = (Cil.visitCilBlock (self :> Cil.cilVisitor) b) in
      ignore(Stack.pop inLoopId);
      let newst = Loop (ca, nb, l, s1, s2) in
      stmt.skind <- newst;
      Cil.ChangeTo stmt
    | _ ->
      Cil.DoChildrenPost (fun stmt ->
          let res =
            if not lbl then
              Stmt.block (!labelUses @ [stmt])
            else begin
              stmt.skind <-Block (Cil.mkBlock (!labelUses @ [Cil.mkStmt stmt.skind]));
              stmt
            end
          in
          labelUses := []; res
        )

  (* Use *)
  method! vexpr expr = match expr.enode with
    | Lval (Var v,offset) ->
      let vid = v.vid in
      if not v.vglob && not (v.vname = "__retres")
         && not v.vtemp && Hashtbl.mem nBVarDefs vid then begin
          let j = (Hashtbl.find currentUse vid) in
          for i = 1 to (Hashtbl.find currentDef vid) - 1  do
            let ids = get_seq_id vid i j in
            self#mkSeq ids vid 2
          done;
          (Hashtbl.replace currentUse vid (j + 1));
          (* même chose que pour les Set (cf. vstmt_aux) *)
          if not (Stack.is_empty inLoopId) then begin
            let lid = Stack.top inLoopId in
            let nvid = get_varLoop_id vid lid in
            if Hashtbl.mem nBVarDefs nvid then begin
              let j = (Hashtbl.find currentUse nvid) in
              for i = (Hashtbl.find currentDef nvid) to (Hashtbl.find nBVarDefs nvid) do
                let ids = get_seq_id nvid i j in
                self#mkSeq ids vid 2
              done;
              Hashtbl.replace currentUse nvid (j + 1)
            end
          end
        end;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

let compute_hl () =
  let regroup = Hashtbl.create 128 in
  let fill (vid,defId,ids) =
    if Hashtbl.mem regroup (vid,defId) then
      Hashtbl.replace regroup (vid,defId) (ids :: (Hashtbl.find regroup (vid,defId)))
    else
      Hashtbl.add regroup (vid,defId) [ids]
  in
  List.iter fill !idList;
  if String.equal "-" !symb then
    Hashtbl.fold (fun _ seqs str ->
        List.fold_left (fun acc s -> acc ^ "<s" ^ string_of_int s ^"|; ;>,\n") "" seqs
      ) regroup ""
  else
    Hashtbl.fold (fun _ seqs str ->
        str ^ "<" ^ (String.concat !symb (List.map (fun s -> "s" ^ string_of_int s) seqs)) ^ "|; ;>,\n"
      ) regroup ""

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl () in
  let out = open_out_gen [Open_creat; Open_append] 0o640 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of labels = %d" ((List.length !idList) *2);
  Options.feedback "finished"

(**
   All-defs annotator
*)
let visite file =
  Visitor.visitFramacFileSameGlobals (new countDefUse :> Visitor.frama_c_visitor) file;
  Visitor.visitFramacFileSameGlobals (new addLabels :> Visitor.frama_c_visitor) file

module AllDefs = Annotators.Register (struct
    let name = "alldefs"
    let help = "All-Definitions Coverage"
    let apply _ file =
      visite file;
      symb := "+";
      gen_hyperlabels ()
  end)


(**
   All-uses annotator
*)
module AllUses = Annotators.Register (struct
    let name = "alluses"
    let help = "All-Uses Coverage"
    let apply _ file =
      visite file;
      symb := ".";
      gen_hyperlabels ()
  end)

(**
   Def-Use annotator
*)
module Defuse = Annotators.Register (struct
    let name = "defuse"
    let help = "Definition-Use Coverage"
    let apply _ file =
      visite file;
      symb := "-";
      gen_hyperlabels ()
  end)
