open Cil_types
open Ast_const

let nBVarDefs = Hashtbl.create 32
let currentDef = Hashtbl.create 32

let multiUses = Hashtbl.create 32
let pathID = Hashtbl.create 32
let invertedPathID = Hashtbl.create 32
let varLoopID = Hashtbl.create 32

let labelUses = ref []
let labelDefs = ref []
let labelStops = ref []
let idList = ref []

let totalLabel = ref 0
let symb = ref ""

(* Attribut un identifiant unique pour une variable dans une boucle *)
let get_varLoop_id vid lid =
  if Hashtbl.mem varLoopID (vid,lid) then
    Hashtbl.find varLoopID (vid,lid)
  else begin
    let new_id = Cil_const.new_raw_id () in
    Hashtbl.add varLoopID (vid,lid) new_id;
    new_id
  end

let rec n_cartesian_product ll =
  match ll with
  | [] -> assert false
  | [l] -> List.fold_left (fun acc i -> [i]::acc) [] l
  | h :: t ->
    let rest = n_cartesian_product t in
    List.concat
      (List.fold_left (fun acc i -> (List.fold_left (fun acc2 r -> (i :: r)::acc2) [] rest)::acc) [] h)

(****************************************)
(* Compte les LVals dans une expression *)
(****************************************)
class multiUseExp = object(self)
  inherit Visitor.frama_c_inplace

  val mutable id_LVals = []

  method get_LVals () = id_LVals

  method! vexpr expr =
    match expr.enode with
    | Lval (Var v,_) ->
      if not v.vglob && not (v.vname = "__retres" ) && not v.vtemp && Hashtbl.mem nBVarDefs v.vid then
        if not (List.exists (fun id -> id = v.vid) id_LVals) then begin
          id_LVals <- v.vid :: id_LVals
        end;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

(** Visitor Count Definitions **)
class countDef = object(self)
  inherit Visitor.frama_c_inplace

  val inLoopId = Stack.create ()

  method fill_aux vid =
    if (Hashtbl.mem nBVarDefs vid) then
      (Hashtbl.replace nBVarDefs vid ((Hashtbl.find nBVarDefs vid) + 1))
    else
      (Hashtbl.add nBVarDefs vid 1; Hashtbl.add currentDef vid 1);

  method fill_def vid =
    self#fill_aux vid;
    if not (Stack.is_empty inLoopId) then begin
      let lid = Stack.top inLoopId in
      let nvid = get_varLoop_id vid lid in
      self#fill_aux nvid
    end

  (* Def-Param *)
  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      let parList = dec.sformals in
      List.iter (fun v ->
          self#fill_aux v.vid
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
            self#fill_def v.vid
          end; f
        )
    | Loop (_,b,_,_,_) ->
      Stack.push stmt.sid inLoopId;
      ignore(Cil.visitCilBlock (self :> Cil.cilVisitor) b);
      ignore(Stack.pop inLoopId);
      Cil.SkipChildren
    | _ -> Cil.DoChildren
end

exception Sorted
(************************************************************)
(* From previously found definitions, extract all paths for *)
(* each expression with more than 1 LVal                    *)
(************************************************************)
class computePaths = object(self)
  inherit Visitor.frama_c_inplace

  val inLoopId = Stack.create ()

  (* Def-Param *)
  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else
      Cil.DoChildren

  (* Def-Var *)
  method! vstmt_aux stmt =
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* Ignorer les labels *)
    | Instr (Set ((Var v,_),_,_))
    | Instr (Call (Some (Var v,_),_,_,_))
    | Instr (Local_init (v,_,_)) ->
      Cil.DoChildrenPost (fun f ->
          if not (v.vname = "__retres") && not v.vtemp then begin
            Hashtbl.replace currentDef v.vid ((Hashtbl.find currentDef v.vid) + 1);
            if not (Stack.is_empty inLoopId) then begin
              let lid = get_varLoop_id v.vid (Stack.top inLoopId) in
              Hashtbl.replace currentDef lid ((Hashtbl.find currentDef lid) + 1);
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
    let vExp = new multiUseExp in
    ignore(Cil.visitCilExpr (vExp :> Cil.cilVisitor) expr);
    let lvalIds = (*List.sort (fun a b -> compare a b) *)(vExp#get_LVals ()) in
    if List.length lvalIds > 1 then begin
      (*Pour chaque lvar je récupère son nombre de def précédent *)
      let alldefs = List.fold_left (fun acc id -> (id, (Hashtbl.find currentDef id) - 1) :: acc ) [] lvalIds in
      (*Je déroule, [(x,3);(y,1)] devient [[(x,1);(x,2);(x,3)];[(y,1)]] ... *)
      let nl = List.fold_left (fun acc (vid,nbDef) ->
          let normalDefs = List.init nbDef (fun idDef -> (vid,idDef+1)) in
          let inLoopDefs =
            if not (Stack.is_empty inLoopId) && Hashtbl.mem varLoopID (vid,Stack.top inLoopId) then begin
              let lid = get_varLoop_id vid (Stack.top inLoopId) in
              let currDef = (Hashtbl.find currentDef lid) in
              let comingNext = (Hashtbl.find nBVarDefs lid) - currDef + 1 in
              let totalDef = Hashtbl.find nBVarDefs vid in
              List.init comingNext (fun idDef -> (vid,totalDef+currDef+idDef))
            end
            else
              []
          in
          (normalDefs@inLoopDefs) :: acc
        ) [] alldefs
      in
      let taille_prod = List.fold_left (fun acc l -> acc * (List.length l)) 1 nl in
      if taille_prod <= Options.MaxContextPath.get () then begin
        (* Liste de toutes les combinaisons possibles de defs *)
        let all_cases = n_cartesian_product nl in
        let f id cc =
          if Hashtbl.mem invertedPathID cc then
            Hashtbl.replace invertedPathID cc (id :: (Hashtbl.find invertedPathID cc))
          else
            Hashtbl.add invertedPathID cc [id]
        in
        let sort l1 l2 =
          let ret = ref 0 in
          try
            List.iter2 (fun (_,c1) (_,c2) ->
              ret := c1 - c2;
              if !ret != 0 then raise Sorted
              ) l1 l2; 0
          with Sorted -> !ret
        in
        let all_cases = List.sort sort all_cases in
        List.iter (fun c ->
            (*J'associe à chaque combinaison un id unique *)
            let new_id = Annotators.next () in
            (* J'associe l'id de l'expr courante à ce nouvel id *)
            if Hashtbl.mem multiUses expr.eid then
              Hashtbl.replace multiUses expr.eid (new_id :: (Hashtbl.find multiUses expr.eid))
            else
              Hashtbl.add multiUses expr.eid [new_id];
            (*J'associe ce nouvel id avec la taille de cet combinaison et un compteur*)
            Hashtbl.add pathID new_id ((List.length c) +1,ref 1);
            (*Chaque couple (Lval,idDef) sera associé a cette combinaison *)
            List.iter (f new_id) c
          ) all_cases
      end
      else
        Options.warning "Expression ignored in file %a, to many paths (%d)" Printer.pp_location expr.eloc taille_prod
    end;
    Cil.SkipChildren
end

(**********************)
(* Visitor Add Labels *)
(**********************)
class addLabels = object(self)
  inherit Visitor.frama_c_inplace

  val inLoopId = Stack.create ()

  method mkSeq ids vid sid lid =
    let idExp = Exp.kinteger IULong ids in
    let oneExp = Exp.one () in
    let curr = Exp.integer sid in
    let slen = Exp.integer lid in
    let varExp = Cil.mkString Cil_datatype.Location.unknown (vid) in
    let zeroExp = Exp.zero () in
    let newStmt = Utils.mk_call "pc_label_sequence" ([oneExp;idExp;curr;slen;varExp;zeroExp]) in
    if sid != lid (*Def*) then
      labelDefs := newStmt :: !labelDefs
    else (* Use *)
      labelUses := newStmt :: !labelUses

  method mkCond vid =
    let zeroExp = Exp.zero () in
    let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int vid))) in
    let newStmt = (Utils.mk_call "pc_label_sequence_condition" ([zeroExp;ccExp])) in
    labelStops := newStmt :: !labelStops

  method handle_param v =
    let i = (Hashtbl.find currentDef v.vid) in
    if Hashtbl.mem invertedPathID (v.vid,i) then begin
      (* Je récupère toutes les expressions ou v apparait avec cette def *)
      let combIds = Hashtbl.find invertedPathID (v.vid,i) in
      Hashtbl.remove invertedPathID (v.vid,i);
      let f idComb =
        let (len,sid) = Hashtbl.find pathID idComb in
        self#mkSeq idComb (string_of_int v.vid) !sid len;
        incr sid
      in
      List.iter f combIds
    end

  (* Def-Param *)
  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      labelDefs := [];
      let parList = dec.sformals in
      List.iter self#handle_param parList;
      let labelParams = List.rev !labelDefs in
      labelDefs := [];
      Cil.DoChildrenPost (fun dec ->
          (dec.sbody.bstmts <- (labelParams @ dec.sbody.bstmts)); dec
        )
    end

  (* Def-Var *)
  method! vstmt_aux stmt =
    let lbl = List.length stmt.labels != 0 in
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* ignorer les labels *)
    | Instr (Set ((Var v,_),_,_))
    | Instr (Call (Some (Var v,_),_,_,_))
    | Instr (Local_init (v,_,_)) ->
      let processSet v =
        let vid = v.vid in
        if not (v.vname = "__retres") && not v.vtemp then begin
          self#mkCond vid;
          let currDef = Hashtbl.find currentDef vid in
          if Hashtbl.mem invertedPathID (vid,currDef) then begin
            (* Je récupère toutes les expressions ou v apparait avec cette def *)
            let combIds = Hashtbl.find invertedPathID (vid,currDef) in
            Hashtbl.remove invertedPathID (vid,currDef);
            let f idComb =
              let (len,sid) = Hashtbl.find pathID idComb in
              self#mkSeq idComb (string_of_int vid) !sid len;
              incr sid
            in
            List.iter f combIds;
          end;
          (Hashtbl.replace currentDef vid (currDef + 1));
          if not (Stack.is_empty inLoopId) then begin
            let lid = Stack.top inLoopId in
            let nvid = get_varLoop_id vid lid in
            let lcurrDef = Hashtbl.find currentDef nvid in
            let oftDef = Hashtbl.find nBVarDefs vid + lcurrDef in
            if Hashtbl.mem invertedPathID (vid,oftDef) then begin
              let combIds = Hashtbl.find invertedPathID (vid,oftDef) in
              Hashtbl.remove invertedPathID (vid,oftDef);
              let f idComb =
                let (len,sid) = Hashtbl.find pathID idComb in
                self#mkSeq idComb (string_of_int vid) !sid len;
                incr sid
              in
              List.iter f combIds
            end;
            Hashtbl.replace currentDef nvid (lcurrDef + 1)
          end;
        end
      in
      Cil.DoChildrenPost (fun stmt ->
          processSet v;
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
    | Loop (_,b,_,_,_) ->
      Stack.push stmt.sid inLoopId;
      ignore(Cil.visitCilBlock (self :> Cil.cilVisitor) b);
      ignore(Stack.pop inLoopId);
      Cil.SkipChildren
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
  method! vexpr expr =
    let eid = expr.eid in
    if Hashtbl.mem multiUses eid then begin
      let combIds = Hashtbl.find multiUses eid in
      let f idComb =
        let (len,sid) = Hashtbl.find pathID idComb in
        self#mkSeq idComb "N/A" len len;
        totalLabel := len + !totalLabel;
        idList := (eid,idComb) :: !idList
      in
      List.iter f combIds
    end;
    Cil.SkipChildren
end

let compute_hl () =
  let regroup = Hashtbl.create 512 in
  let fill (eid,idComb) =
    if Hashtbl.mem regroup eid then
      Hashtbl.replace regroup eid (idComb :: (Hashtbl.find regroup eid))
    else
      Hashtbl.add regroup eid [idComb]
  in
  List.iter fill !idList;
  Hashtbl.fold (fun _ seqs str -> str ^ "<" ^ (String.concat !symb (List.rev (List.map (fun s -> "s" ^ string_of_int s) seqs))) ^ "|; ;>,\n") regroup ""

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl () in (*(Hashtbl.fold (fun id vals str -> str ^ (compute_hl vals)) multiUses "") in*)
  let out = open_out_gen [Open_creat; Open_append] 0o640 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of labels = %d" !totalLabel;
  Options.feedback "finished"

(**
   All-defs annotator
*)
let visite file =
  Visitor.visitFramacFileSameGlobals (new countDef :> Visitor.frama_c_visitor) file;
  Visitor.visitFramacFileSameGlobals (new computePaths :> Visitor.frama_c_visitor) file;
  Hashtbl.iter (fun k v -> Hashtbl.replace currentDef k 1) currentDef;
  Visitor.visitFramacFileSameGlobals (new addLabels :> Visitor.frama_c_visitor) file

module Context = Annotators.Register (struct
    let name = "context"
    let help = "Context Coverage"
    let apply _ file =
      Options.result "[WIP] Context is currently in Alpha";
      visite file;
      symb := "+";
      gen_hyperlabels ()
  end)
