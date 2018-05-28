open Cil_types
open Ast_const

let nBVarDefs = Hashtbl.create 100
let nBVarUses = Hashtbl.create 100
let nBInLoopDefs = Hashtbl.create 10
let nBInLoopUses = Hashtbl.create 10

let currentDef = Hashtbl.create 100
let currentUse = Hashtbl.create 100
let currentInLoopDef = Hashtbl.create 100
let currentInLoopUse = Hashtbl.create 100

let pairVarOffset = Hashtbl.create 100

let cantor_pairing n m = (((n+m)*(n+m+1))/2)+m
let get_seq_id varId def use = cantor_pairing varId (cantor_pairing def use)

let get_index_from_offset offset =
  match offset with
  | Index (e,_) ->
    let i = Cil.constFoldToInt e in
    (match i with
    | None -> None
    | Some(int) -> Some(Integer.to_int int))
  | _ -> None

let get_index_from_skind skind =
  match skind with
  | Instr (Set ((Var _,offset),_,_))
  | Instr (Call (Some (Var _,offset),_,_,_)) ->
    get_index_from_offset offset
  | _ -> None

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

(** All-defs Visitor Count **)
class visitor = object(self)
  inherit Visitor.frama_c_inplace

  val mutable current_func = ""

  val inLoopId = Stack.create ()

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
            (Hashtbl.add nBVarDefs v.vid 1; Hashtbl.add currentDef v.vid 1)
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
          if not (v.vname = "__retres") && not v.vtemp then begin
            let rvid = get_rvid_skind v.vid stmt.skind in
            if (Hashtbl.mem nBVarDefs rvid) then
              (Hashtbl.replace nBVarDefs rvid ((Hashtbl.find nBVarDefs rvid) + 1))
            else
              (Hashtbl.add nBVarDefs rvid 1; Hashtbl.add currentDef rvid 1);
            (*Dans le cas ou le Set se trouve dans une loop, on fait la même chose mais avec des hashtbl pour def/use internes aux boucles *)
            if not (Stack.is_empty inLoopId) then begin
              let idl = Stack.top inLoopId in
              let id = cantor_pairing idl rvid in
              if (Hashtbl.mem nBInLoopDefs id) then
                (Hashtbl.replace nBInLoopDefs id ((Hashtbl.find nBInLoopDefs id) + 1))
              else
                (Hashtbl.add nBInLoopDefs id 1; Hashtbl.add currentInLoopDef id 1)
            end
          end; f
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
      if not v.vglob && not (v.vname = "__retres" ) && not v.vtemp then begin
        let rvid = get_rvid_offset v.vid offset in
        if (Hashtbl.mem nBVarUses rvid) then
          (Hashtbl.replace nBVarUses rvid ((Hashtbl.find nBVarUses rvid) + 1))
        else
          (Hashtbl.add nBVarUses rvid 1; Hashtbl.add currentUse rvid 1);
            (*Dans le cas ou le Use se trouve dans une loop, on fait la même chose mais avec des hashtbl pour def/use interne aux boucles *)
        if not (Stack.is_empty inLoopId) then begin
          let idl = Stack.top inLoopId in
          let id = cantor_pairing idl rvid in
          if (Hashtbl.mem nBInLoopUses id) then
            (Hashtbl.replace nBInLoopUses id ((Hashtbl.find nBInLoopUses id) + 1))
          else
            (Hashtbl.add nBInLoopUses id 1; Hashtbl.add currentInLoopUse id 1)
        end
      end;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

let labelUses = ref []
let labelDefs = ref []
let labelStops = ref []
let idList = ref []

let handle_param v =
  if Hashtbl.mem nBVarUses v.vid then begin
    let i = (Hashtbl.find currentDef v.vid) in
    for j = (Hashtbl.find currentUse v.vid) to (Hashtbl.find nBVarUses v.vid) do (* OPTIM : only labels for previous defs/next uses *)
      let ids = get_seq_id v.vid i j in
      idList := (v.vid,i,ids) :: !idList;
      let idExp = Exp.integer ids in
      let oneExp = Exp.one () in
      let twoExp = Exp.one () in
      let twoExptwo = Exp.integer 2 in
      let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  v.vid))) in
      let zeroExp = Exp.zero () in
      let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
      labelDefs := newStmt :: !labelDefs;
    done;
    Hashtbl.replace currentDef v.vid (i + 1)
  end

(** All-defs Visitor Add Labels **)
class visitorTwo = object(self)
  inherit Visitor.frama_c_inplace

  val inLoopId = Stack.create ()
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
    let lbl = List.length stmt.labels != 0 in
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* ignorer les labels *)
    | Instr (Set ((Var v,_),_,_))
    | Instr (Call (Some (Var v,_),_,_,_))
    | Instr (Local_init (v,_,_)) ->
      (* Les labels pour le set sont fait en post pour éviter les erreurs de labels dans les statements ou la variable qui
         est set estégalement use (i++ par exemple) *)
      let processSet v =
        labelStops := [];
        labelDefs := [];
        let rvid = get_rvid_skind v.vid stmt.skind in
        if not (v.vname = "__retres") && not v.vtemp && Hashtbl.mem nBVarUses rvid then begin
          let zeroExp = Exp.zero () in
          let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int rvid))) in
          let newStmt = (Utils.mk_call "pc_label_sequence_condition" ([zeroExp;ccExp])) in
          labelStops := newStmt :: !labelStops;
          let defId = (Hashtbl.find currentDef rvid) in
          for j = (Hashtbl.find currentUse rvid) to (Hashtbl.find nBVarUses rvid) do
            let ids = get_seq_id rvid defId j in
            idList := (rvid,defId,ids) :: !idList;
            let idExp = Exp.integer ids in
            let oneExp = Exp.one () in
            let twoExp = Exp.one () in
            let twoExptwo = Exp.integer 2 in
            let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  rvid))) in
            let zeroExp = Exp.zero () in
            let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
            labelDefs := newStmt :: !labelDefs
          done;
          (Hashtbl.replace currentDef rvid (defId + 1));

          (* On procède presque de la même manière que les def/use normaux, sauf qu'au lieu de faire :
             - pour une definition donnée ajouter un label pour chaque use qui suit
             on fait :
             - pour une definition donnée dans une boucle ajouter un label pour chaque use la précédent DANS la boucle
             Ainsi avec un code de type :
             While(1){
               if (!(i<10)) break;
               i++;
             }
             On vérifiera que la définition de i avec i++ est bien utilisée dans l'itération suivante
          *)
          if not (Stack.is_empty inLoopId) then begin
            let idl = Stack.top inLoopId in
            let id = cantor_pairing idl rvid in
            if Hashtbl.mem nBInLoopUses id then begin
              let offseti = (Hashtbl.find nBVarDefs rvid) in
              let offsetj = (Hashtbl.find nBVarUses rvid) in
              let i = (Hashtbl.find currentInLoopDef id) in
              for j = 1 to (Hashtbl.find currentInLoopUse id) - 1 do
                let ids = get_seq_id rvid (i+offseti) (j+offsetj) in
                idList := (rvid,defId,ids) :: !idList;
                let idExp = Exp.integer ids in
                let oneExp = Exp.one () in
                let twoExp = Exp.one () in
                let twoExptwo = Exp.integer 2 in
                let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  rvid))) in
                let zeroExp = Exp.zero () in
                let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
                labelDefs := newStmt :: !labelDefs
              done;
              Hashtbl.replace currentInLoopDef id (i + 1)
            end
          end
        end
      in
      Cil.DoChildrenPost (fun stmt ->
          processSet v;
          let res =
            (* Si un stmt possède 1 ou plusieurs labels (labels C),
               les labels (labels Ltest) qui correspondent au stmt
               seront insérés entre le/les labels labels C et le stmt *)
            if not lbl then
              Stmt.block (!labelUses @ !labelStops @ [stmt] @ !labelDefs)
            else
              Stmt.block ({stmt with skind = Block (Block.mk (!labelUses @ !labelStops @ [Cil.mkStmt stmt.skind]))} :: !labelDefs)
          in
          labelUses := [];
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
            (* Si un stmt possède 1 ou plusieurs labels (labels C),
               les labels (labels Ltest) qui correspondent au stmt
               seront insérés entre le/les labels labels C et le stmt *)
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
      let rvid = get_rvid_offset v.vid offset in
      if not v.vglob && not (v.vname = "__retres") && not v.vtemp && Hashtbl.mem nBVarDefs rvid then begin
          let j = (Hashtbl.find currentUse rvid) in
          for i = 1 to (Hashtbl.find currentDef rvid) - 1  do
            let id = get_seq_id rvid i j in
            let idExp = Exp.integer id in
            let oneExp = Exp.one () in
            let twoExp = Exp.integer 2 in
            let twoExptwo = Exp.integer 2 in
            let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int  rvid))) in
            let zeroExp = Exp.zero () in
            let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
            labelUses := newStmt :: !labelUses
          done;
          (Hashtbl.replace currentUse rvid (j + 1));

          (* même chose que pour les Set (cf. vstmt_aux) *)
          if not (Stack.is_empty inLoopId) then begin
            let idl = Stack.top inLoopId in
            let id = cantor_pairing idl rvid in
            if Hashtbl.mem nBInLoopDefs id then begin
              let offseti = (Hashtbl.find nBVarDefs rvid) in
              let offsetj = (Hashtbl.find nBVarUses rvid) in
              let j = (Hashtbl.find currentInLoopUse id) in
              for i = (Hashtbl.find currentInLoopDef id) to (Hashtbl.find nBInLoopDefs id) do
                let ids = get_seq_id rvid (i+offseti) (j+offsetj) in
                let idExp = Exp.integer ids in
                let oneExp = Exp.one () in
                let twoExp = Exp.integer 2 in
                let twoExptwo = Exp.integer 2 in
                let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int rvid))) in
                let zeroExp = Exp.zero () in
                let newStmt = (Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])) in
                labelUses := newStmt :: !labelUses
              done;
              Hashtbl.replace currentInLoopUse id (j + 1)
            end
          end
        end;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

let symb = ref ""
let temp = ref ""

let compute_hl id =
  if (Hashtbl.mem nBVarDefs id) then begin
    for k = 1 to (Hashtbl.find nBVarDefs id) do
      let ll = List.fold_left (fun acc (idv,defId,ids) -> if idv = id && defId = k then ids::acc else acc) [] !idList in
      if ll != [] then begin
        if String.equal "-" !symb then begin
          temp := !temp ^ (String.concat "" (List.map (fun i -> "<s" ^ string_of_int i ^"|; ;>,\n") ll))
        end
        else
          temp := !temp ^ "<"
                  ^ (String.concat !symb (List.map (fun i -> "s" ^ string_of_int i) ll))
                  ^ "|; ;>,\n"
      end
    done;
    !temp
  end
  else ""

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = (Hashtbl.fold (fun id _ str -> temp := ""; (compute_hl id) ^ str) nBVarUses "") in
  let out = open_out_gen [Open_creat; Open_append] 0o640 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of labels = %d" ((List.length !idList) *2);
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
