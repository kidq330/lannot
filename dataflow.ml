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
    | Instr (Set ((Var v,_),_,_))  | Instr (Call (Some (Var v,_),_,_,_))  ->
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
        let oneExp = (Cil.integer Cil_datatype.Location.unknown 1) in
        let id = get_seq_id v.vid i j in
        idList := id :: !idList;
        let idExp = (Cil.integer Cil_datatype.Location.unknown id) in
        let twoExp = (Cil.integer Cil_datatype.Location.unknown 1) in
        let twoExptwo = (Cil.integer Cil_datatype.Location.unknown 2) in
        let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  v.vid))) in
        let zeroExp = (Cil.integer Cil_datatype.Location.unknown 0) in
        let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
        labelDefs := newStmt :: !labelDefs
      done;
      (Hashtbl.replace currentDef v.vid (i + 1))
    end
  end



(** All-defs Visitor Add Labels **)
class visitorTwo = object(_)
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
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* ignorer les labels *)
    | Instr (Set ((Var v,_),_,_)) | Instr (Call (Some (Var v,_),_,_,_)) ->
      labelStops := [];
      if not (v.vname = "__retres") && Hashtbl.mem nBVarUses v.vid
         && not ((String.sub v.vname 0 (min 3 (String.length v.vname))) = "tmp") then begin
        (* OPTIM : un seul stop pour une variable *)
        (* for i = 1 to (Hashtbl.find nBVarDefs v.vid) do
           for j = 1 to (Hashtbl.find nBVarUses v.vid) do *)
        let oneExp = (Cil.integer Cil_datatype.Location.unknown 0) in
        let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  v.vid) (* v.vname ^ " - stop ") *))) in
        let newStmt = (Utils.mk_call "pc_label_sequence_condition" ([oneExp;ccExp])) in
        labelStops := newStmt :: !labelStops
        (* done; done; *)
      end;
      labelDefs := [];
      if not (v.vname = "__retres") && Hashtbl.mem nBVarUses v.vid
         && not ((String.sub v.vname 0 (min 3 (String.length v.vname))) = "tmp") then begin
        let i = (Hashtbl.find currentDef v.vid) in begin
          for j = (Hashtbl.find currentUse v.vid) to (Hashtbl.find nBVarUses v.vid) do
            let oneExp = (Cil.integer Cil_datatype.Location.unknown 1) in
            let id = get_seq_id v.vid i j in
            idList := id :: !idList;
            let idExp = (Cil.integer Cil_datatype.Location.unknown id) in
            let twoExp = (Cil.integer Cil_datatype.Location.unknown 1) in
            let twoExptwo = (Cil.integer Cil_datatype.Location.unknown 2) in
            let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  v.vid))) in
            let zeroExp = (Cil.integer Cil_datatype.Location.unknown 0) in
            let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
            labelDefs := newStmt :: !labelDefs
          done;
          (Hashtbl.replace currentDef v.vid (i + 1))
        end;
      end;
      Cil.DoChildrenPost (fun stmt -> let res = (Stmt.block (!labelUses @ !labelStops @ [stmt] @ !labelDefs)) in labelUses := []; res)
    | If (ex,th,el,lo) ->
      ignore(Cil.visitCilExpr (new visitorTwo :> Cil.cilVisitor) ex);
      (let lu = !labelUses in labelUses := [];
       let newSt = (Cil.mkBlock (lu @ [Cil.mkStmt (If (ex,
                                                       (Cil.visitCilBlock (new visitorTwo :> Cil.cilVisitor) th),
                                                       (Cil.visitCilBlock (new visitorTwo :> Cil.cilVisitor) el),
                                                       lo)
                                                  )
                                      ]
                                )
                   ) in stmt.skind <- (Block newSt);
       Cil.ChangeTo stmt)
    | _ -> Cil.DoChildrenPost (fun stmt -> let res = (Stmt.block (!labelUses @ [stmt])) in labelUses := []; res)


  (* Use *)
  method! vexpr expr = match expr.enode with
    | Lval (Var v,_) ->
      if not v.vglob && not (v.vname = "__retres") && (Hashtbl.mem nBVarDefs v.vid)
         && not ((String.sub v.vname 0 (min 3 (String.length v.vname))) = "tmp") then begin
        let j = (Hashtbl.find currentUse v.vid) in
        begin
          for i = 1 to (Hashtbl.find currentDef v.vid) - 1 (* (Hashtbl.find nBVarDefs v.vid) *) do
            let oneExp = (Cil.integer Cil_datatype.Location.unknown 1) in
            let id = get_seq_id v.vid i j in
            idList := id :: !idList;
            let idExp = (Cil.integer Cil_datatype.Location.unknown id) in
            let twoExp = (Cil.integer Cil_datatype.Location.unknown 2) in
            let twoExptwo = (Cil.integer Cil_datatype.Location.unknown 2) in
            let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  v.vid))) in
            let zeroExp = (Cil.integer Cil_datatype.Location.unknown 0) in
            let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
            labelUses := newStmt :: !labelUses
          done;
          (Hashtbl.replace currentUse v.vid (j + 1))
        end
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
      temp := !temp ^ "<"
              ^ (String.concat !symb (List.map (fun i -> "s" ^ string_of_int i) (get_list_labels k id)))
              ^ "|; ;>,\n"
    done;
    !temp
  end
  else ""

let gen_hyperlabels = ref (fun () ->
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let out = open_out data_filename in
  output_string out (Hashtbl.fold (fun id nb str -> ignore nb; temp := ""; (compute_hl id) ^ str) nBVarUses "");
  close_out out;
  Options.feedback "Total number of labels = %d" (!nbLabels*2);
  Options.feedback "finished")

let alreadyDone = ref false

(**
   All-defs annotator
*)

let visite file =
  if not !alreadyDone then begin
    Visitor.visitFramacFileSameGlobals (new visitor :> Visitor.frama_c_visitor) file;
    Visitor.visitFramacFileSameGlobals (new visitorTwo :> Visitor.frama_c_visitor) file;
    alreadyDone := true
  end

module AllDefs = Annotators.Register (struct
    let name = "alldefs"
    let help = "All-Definitions Coverage"
    let apply mk_label file =
      ignore mk_label; (* Avoid warning about mk_label unused *)
      visite file;
      symb := "+";
      !gen_hyperlabels ()
  end)


(**
   All-uses annotator
*)
module AllUses = Annotators.Register (struct
    let name = "alluses"
    let help = "All-Uses Coverage"
    let apply mk_label file =
      ignore mk_label; (* Avoid warning about mk_label unused *)
      visite file;
      symb := ".";
      !gen_hyperlabels ()
  end)
