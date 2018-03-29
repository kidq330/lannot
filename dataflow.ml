open Cil_types
open Ast_const

let nBVarDefs = Hashtbl.create 100
let nBVarUses = Hashtbl.create 100

let currentDef = Hashtbl.create 100
let currentUse = Hashtbl.create 100

(** All-defs Visitor Count **)
class visitor = object(_)
  inherit Visitor.frama_c_inplace

  val mutable current_func = ""

  (* Def-Param *)
  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      let parList = dec.sformals in
      List.iter (fun v ->
          if (Hashtbl.mem nBVarDefs v.vid) then
            (Hashtbl.replace nBVarDefs v.vid ((Hashtbl.find nBVarDefs v.vid) + 1))
          else
            (Hashtbl.add nBVarDefs v.vid 1); (Hashtbl.add currentDef v.vid 1)
        ) parList;
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
          if not (v.vname = "__retres") && not ((String.sub v.vname 0 (min 3 (String.length v.vname))) = "tmp") then begin
            if (Hashtbl.mem nBVarDefs v.vid) then
              (Hashtbl.replace nBVarDefs v.vid ((Hashtbl.find nBVarDefs v.vid) + 1))
            else
              (Hashtbl.add nBVarDefs v.vid 1); (Hashtbl.add currentDef v.vid 1)
          end; f
        )
    | _ -> Cil.DoChildren

  (* Use *)
  method! vexpr expr =
    match expr.enode with
    | Lval (Var v,_) ->
      if not v.vglob && not (v.vname = "__retres" ) && not ((String.sub v.vname 0 (min 3 (String.length v.vname))) = "tmp") then begin
        if (Hashtbl.mem nBVarUses v.vid) then
          (Hashtbl.replace nBVarUses v.vid ((Hashtbl.find nBVarUses v.vid) + 1))
        else
          (Hashtbl.add nBVarUses v.vid 1); (Hashtbl.add currentUse v.vid 1)
      end;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

let cantor_pairing n m = (((n+m)*(n+m+1))/2)+m
let get_seq_id varId def use = cantor_pairing varId (cantor_pairing def use)

let labelUses = ref []
let labelDefs = ref []
let labelStops = ref []
let idList = ref []

let handle_param v =
  if Hashtbl.mem nBVarUses v.vid then begin
    let i = (Hashtbl.find currentDef v.vid) in begin
      for j = (Hashtbl.find currentUse v.vid) to (Hashtbl.find nBVarUses v.vid) do (* OPTIM : only labels for previous defs/next uses *)
        let id = get_seq_id v.vid i j in
        let oneExp = (Cil.integer Cil_datatype.Location.unknown 1) in
        idList := id :: !idList;
        let idExp = (Cil.integer Cil_datatype.Location.unknown id) in
        let twoExp = (Cil.integer Cil_datatype.Location.unknown 1) in
        let twoExptwo = (Cil.integer Cil_datatype.Location.unknown 2) in
        let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  v.vid))) in
        let zeroExp = (Cil.integer Cil_datatype.Location.unknown 0) in
        let prefix = if Options.Debug.get () != 0 then "set_" else "" in
        let newStmt = (Utils.mk_call (prefix^"pc_label_sequence") ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
        labelDefs := newStmt :: !labelDefs;
      done;
      (Hashtbl.replace currentDef v.vid (i + 1))
    end
  end

(** All-defs Visitor Add Labels **)
class visitorTwo = object(self)
  inherit Visitor.frama_c_inplace

  (* Def-Param *)
  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      labelDefs := [];
      let parList = dec.sformals in
      List.iter handle_param parList;
      let labelParams = !labelDefs in
      Cil.DoChildrenPost (fun dec -> (dec.sbody.bstmts <- (labelParams @ dec.sbody.bstmts));  dec)
    end

  (* Def-Var *)
  method! vstmt_aux stmt =
    let lbl = List.exists (fun l ->
        match l with
        | Case _ -> true
        | Default _ -> true
        | _ -> false
      ) stmt.labels
    in
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* ignorer les labels *)
    | Instr (Set ((Var v,_),_,_))
    | Instr (Call (Some (Var v,_),_,_,_))
    | Instr (Local_init (v,_,_)) ->
      labelStops := [];
      if not (v.vname = "__retres") && Hashtbl.mem nBVarUses v.vid
         && not ((String.sub v.vname 0 (min 3 (String.length v.vname))) = "tmp") then begin
        let oneExp = (Cil.integer Cil_datatype.Location.unknown 0) in
        let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  v.vid))) in
        let newStmt = (Utils.mk_call "pc_label_sequence_condition" ([oneExp;ccExp])) in
        labelStops := newStmt :: !labelStops
      end;
      labelDefs := [];
      if not (v.vname = "__retres") && Hashtbl.mem nBVarUses v.vid
         && not ((String.sub v.vname 0 (min 3 (String.length v.vname))) = "tmp") then begin
        let i = (Hashtbl.find currentDef v.vid) in begin
          for j = (Hashtbl.find currentUse v.vid) to (Hashtbl.find nBVarUses v.vid) do
            let id = get_seq_id v.vid i j in
            let oneExp = (Cil.integer Cil_datatype.Location.unknown 1) in
            idList := id :: !idList;
            let idExp = (Cil.integer Cil_datatype.Location.unknown id) in
            let twoExp = (Cil.integer Cil_datatype.Location.unknown 1) in
            let twoExptwo = (Cil.integer Cil_datatype.Location.unknown 2) in
            let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  v.vid))) in
            let zeroExp = (Cil.integer Cil_datatype.Location.unknown 0) in
            let prefix = if Options.Debug.get () != 0 then "set_" else "" in
            let newStmt = (Utils.mk_call (prefix^"pc_label_sequence") ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
            labelDefs := newStmt :: !labelDefs
          done;
          (Hashtbl.replace currentDef v.vid (i + 1))
        end;
      end;
      (* Dans le cas d'un case (de switch par exemple) on veut que les uses et stops soient après le label, mais avant le skind*)
      if not lbl then
        Cil.DoChildrenPost (fun stmt -> let res = (Stmt.block (!labelUses @ !labelStops @ [stmt] @ !labelDefs)) in labelUses := []; res)
      else
        Cil.DoChildrenPost (fun stmt -> let res = Stmt.block ({stmt with skind = Block (Cil.mkBlock (!labelUses @ !labelStops @ [Cil.mkStmt stmt.skind]))} :: !labelDefs) in labelUses := []; res)
    | If (ex,th,el,lo) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) ex);
      (let lu = !labelUses in labelUses := [];
       let thenb = (Cil.visitCilBlock (self :> Cil.cilVisitor) th) in
       let elseb = (Cil.visitCilBlock (self :> Cil.cilVisitor) el) in
       let newSt = (Cil.mkBlock (lu @ [Cil.mkStmt (If (ex,thenb,elseb,lo))])) in stmt.skind <- (Block newSt);
       Cil.ChangeTo stmt)
    | Switch (ex, b, stmtl, lo) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) ex);
      (let lu = !labelUses in labelUses := [];
       let nb = (Cil.visitCilBlock (self :> Cil.cilVisitor) b) in
       let newSt = (Cil.mkBlock (lu @ [Cil.mkStmt (Switch (ex,nb,stmtl,lo))])) in stmt.skind <- (Block newSt);
       Cil.ChangeTo stmt)
    | _ -> Cil.DoChildrenPost (fun stmt -> let res = (Stmt.block (!labelUses @ [stmt])) in labelUses := []; res)


  (* Use *)
  method! vexpr expr = match expr.enode with
    | Lval (Var v,_) ->
      if not v.vglob && not (v.vname = "__retres") && (Hashtbl.mem nBVarDefs v.vid)
         && not ((String.sub v.vname 0 (min 3 (String.length v.vname))) = "tmp") then begin
        let j = (Hashtbl.find currentUse v.vid) in
        begin
          for i = 1 to (Hashtbl.find currentDef v.vid) - 1  do
            let id = get_seq_id v.vid i j in
            let oneExp = (Cil.integer Cil_datatype.Location.unknown 1) in
            idList := id :: !idList;
            let idExp = (Cil.integer Cil_datatype.Location.unknown id) in
            let twoExp = (Cil.integer Cil_datatype.Location.unknown 2) in
            let twoExptwo = (Cil.integer Cil_datatype.Location.unknown 2) in
            let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  v.vid))) in
            let zeroExp = (Cil.integer Cil_datatype.Location.unknown 0) in
            let prefix = if Options.Debug.get () != 0 then "use_" else "" in
            let newStmt = (Utils.mk_call (prefix^"pc_label_sequence") ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
            labelUses := newStmt :: !labelUses
          done;
          (Hashtbl.replace currentUse v.vid (j + 1))
        end;
      end;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

let listLabels = ref []

let nbLabels = ref 0

let get_list_labels i id =
  listLabels := [];
  for j = 1 to (Hashtbl.find nBVarUses id) do
    let lid = get_seq_id id i j in
    if List.exists (fun lblId -> lid = lblId) !idList then
      listLabels := lid :: !listLabels;
  done;
  nbLabels:= !nbLabels + (List.length !listLabels);
  !listLabels


let symb = ref ""
let temp = ref ""

let compute_hl id =
  if (Hashtbl.mem nBVarDefs id) then begin
    for k = 1 to (Hashtbl.find nBVarDefs id) do
      let ll = get_list_labels k id in
      if ll != [] then
        temp := !temp ^ "<"
                ^ (String.concat !symb (List.map (fun i -> "s" ^ string_of_int i) ll))
                ^ "|; ;>,\n";
    done;
    !temp
  end
  else ""

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = (Hashtbl.fold (fun id _ str -> temp := ""; (compute_hl id) ^ str) nBVarUses "") in
  let out = open_out data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of labels = %d" (!nbLabels*2);
  Options.feedback "finished"

(**
   All-defs annotator
*)

let visite file =
  Visitor.visitFramacFileSameGlobals (new visitor :> Visitor.frama_c_visitor) file;
  Visitor.visitFramacFileSameGlobals (new visitorTwo :> Visitor.frama_c_visitor) file

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
