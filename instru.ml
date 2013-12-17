open Cil
open Cil_types
open Lexing

let nextLabelId : int ref = ref 1

(* (file,line, id, cond) *)
let labelsList : (string*int*int*exp) list ref = ref []


(* val mk_call: loc -> lval option -> string -> exp list -> stmt *)
let mk_call loc ?result fname args =
  let new_lval loc v = new_exp loc (Lval (var v)) in
  let t = match result with
    | Some(Var(v),_) -> v.vtype
    | _ -> voidType in
  let ty = TFun(t, None, false, []) in
  let f = new_lval loc (makeGlobalVar fname ty) in
  mkStmt ~valid_sid:true (Instr(Call(result, f, args, loc)))

let makeLabel cond loc = 
  let labelIdExp = dummy_exp (Const(CInt64(Integer.of_int(!nextLabelId),IInt,None))) in
  let lineNumber = (fst loc).pos_lnum in
  let fileName = (fst loc).pos_fname in
    labelsList := (fileName,lineNumber,!nextLabelId, cond)::!labelsList;
    nextLabelId := !nextLabelId+1;
    mk_call loc "pc_label" [ cond; labelIdExp ]

let rec genLabelPerExp exp loc =
  match exp.enode with
    | BinOp(LAnd, e1, e2, _) ->
        List.append (genLabelPerExp e1 loc) (genLabelPerExp e2 loc)

    | BinOp(LOr, e1, e2, _) ->
        List.append (genLabelPerExp e1 loc) (genLabelPerExp e2 loc)

    | BinOp(Lt, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Ge, e1, e2, t)) in
          [makeLabel exp loc; makeLabel nonExp loc]

    | BinOp(Gt, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Le, e1, e2, t)) in
	  [makeLabel exp loc; makeLabel nonExp loc]

    | BinOp(Le, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Gt, e1, e2, t)) in
	  [makeLabel exp loc; makeLabel nonExp loc]


    | BinOp(Ge, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Lt, e1, e2, t)) in
	  [makeLabel exp loc; makeLabel nonExp loc]

    | BinOp(Eq, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Ne, e1, e2, t)) in
	  [makeLabel exp loc; makeLabel nonExp loc]

    | BinOp(Ne, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Eq, e1, e2, t)) in
	  [makeLabel exp loc; makeLabel nonExp loc]

    | BinOp(_, _e1, _e2, _) ->
        let nonExp = dummy_exp(UnOp(LNot, exp, intType)) in
	  [makeLabel exp loc; makeLabel nonExp loc]

    | Lval(_lv) ->
        let nonExp = dummy_exp(UnOp(LNot, exp, intType)) in
	  [makeLabel exp loc; makeLabel nonExp loc]


    | UnOp(LNot, e1, _) ->
        genLabelPerExp e1 loc

    | _ -> []


class genLabelsVisitor = object(_self)
  inherit Visitor.frama_c_inplace

  method! vstmt_aux stmt =
    let genLabels s =
      match s.skind with

        | If(e, _, _, _) ->
            let loc = Utils.get_stmt_loc s in
            let finalList = List.append  (genLabelPerExp e loc) ([s]) in
            let b2 = mkBlock finalList in
            let i = mkStmt (Block(b2)) in
              i


        | _ -> s
    in
      match stmt.skind with
        | If _ ->
            (*DoChildren *)
            ChangeDoChildrenPost (stmt, genLabels)
        | _ -> DoChildren
end

(* ******************************************************* *)
(* ******************************************************* *)
(* pow(2,10) = 2^10 *)
let rec pow(n, x) =
  if x=0 then
    1
  else
    n * pow(n, x-1);;

let rec getConditionsNumber exp =
  match exp.enode with
    | BinOp(LAnd, e1, e2, _) ->
        (getConditionsNumber e1) + (getConditionsNumber e2)
    | BinOp(LOr, e1, e2, _) ->
        (getConditionsNumber e1) + (getConditionsNumber e2)
    | BinOp(_, _e1, _e2, _) -> 1

    | Lval(_lv) -> 1

    | UnOp(LNot, e1, _) -> getConditionsNumber e1

    | _ -> 0



let generateStatementFromConditionsList conds loc =
  let rec mergeConds condsList =
    match condsList with
      (* | [] -> nil*)
      | a::[] -> a
      | a::b::tail->
          let newExp = dummy_exp(BinOp(LAnd, a, b, intType)) in
            mergeConds (List.append [newExp] tail)
  in
    match conds with
      | [] ->
          let stmt = mk_call loc "pc_label" [ ] in
            stmt
      | _ ->
          let newCond = mergeConds conds in
          let stmt = mk_call loc "pc_label" [ newCond ] in
            stmt

let getNegativeCond exp =
  match exp.enode with
    | BinOp(Lt, e1, e2, t) ->
        dummy_exp(BinOp(Ge, e1, e2, t))

    | BinOp(Gt, e1, e2, t) ->
        dummy_exp(BinOp(Le, e1, e2, t))

    | BinOp(Le, e1, e2, t) ->
        dummy_exp(BinOp(Gt, e1, e2, t))

    | BinOp(Ge, e1, e2, t) ->
        dummy_exp(BinOp(Lt, e1, e2, t))

    | BinOp(Eq, e1, e2, t) ->
        dummy_exp(BinOp(Ne, e1, e2, t))

    | BinOp(Ne, e1, e2, t) ->
        dummy_exp(BinOp(Eq, e1, e2, t))

    | Lval(_lv) ->
        dummy_exp(UnOp(LNot, exp, intType))

    | UnOp(LNot, e1, _) -> e1

    | _ -> Cil.dummy_exp(UnOp(LNot, exp, Cil.intType))

let rec genMultiLabel allConds conditionsNb currentNb newConds cumul loc =
  match allConds with
    | [] ->
        generateStatementFromConditionsList newConds loc

    | a :: tail ->
        (* let ex = pow(2, conditionsNb) in *)
        (* Format.printf "+++++5-3- %d@." ex; *)
        if (currentNb - cumul) / ( pow(2, conditionsNb)) = 1 then
          begin
            let curCond = a in
            let newCumul = cumul + ( pow(2, conditionsNb) ) in
            let newNewConds = List.append newConds [curCond] in
              genMultiLabel tail (conditionsNb-1) currentNb newNewConds newCumul loc
          end
        else
          begin
            let curCond = getNegativeCond a in
            let newCumul = cumul in
            let newNewConds = List.append newConds [curCond] in
              genMultiLabel tail (conditionsNb-1) currentNb newNewConds newCumul loc
          end

let rec genMultiLabels allConds conditionsNb currentNb labelStmts loc =
  let casesNb = pow(2, conditionsNb) in
    if currentNb = casesNb then
      begin
        labelStmts
      end
    else
      begin
        let curStmt = genMultiLabel allConds (conditionsNb-1) currentNb [] 0 loc in
          let newStmts = List.append labelStmts [curStmt] in
            genMultiLabels allConds conditionsNb (currentNb+1) newStmts loc
      end


let rec getUnitaryConditionsInExp exp =
  match exp.enode with
  | BinOp(LAnd, e1, e2, _) ->
      List.append (getUnitaryConditionsInExp e1) (getUnitaryConditionsInExp e2)
  | BinOp(LOr, e1, e2, _) ->
      List.append (getUnitaryConditionsInExp e1) (getUnitaryConditionsInExp e2)
  | BinOp(_, _e1, _e2, _) -> [exp]

  | Lval(_lv) -> [exp]

  | UnOp(LNot, e1, _) -> getUnitaryConditionsInExp e1

  | _ -> []


let prepareLabelsBuffer labelsList myBuffer = 
  Buffer.add_string myBuffer "<labels>\n";

  let rec printLabels labels =
    match labels with
      |	(fileName,lineNb, id, _cond)::rest ->
	  Buffer.add_string myBuffer ("<label>\n");
	  Buffer.add_string myBuffer ("<id>" ^ (string_of_int id)  ^ "</id>\n");
	  Buffer.add_string myBuffer ("<cond>" ^ "</cond>\n");
	  Buffer.add_string myBuffer ("<file>" ^ fileName  ^ "</file>\n");
	  Buffer.add_string myBuffer ("<line>" ^ (string_of_int lineNb)  ^ "</line>\n");
	  Buffer.add_string myBuffer ("</label>\n");
	  printLabels rest 
      | [] -> ()
  in 
    printLabels (List.rev !labelsList);
    Buffer.add_string myBuffer "</labels>\n"



(*****************************************)

class genMultiLabelsVisitor = object(_self)
  inherit Visitor.frama_c_inplace

  method! vstmt_aux stmt =
    let genLabels s =
      match s.skind with
      | If(e, _, _, _) ->
        let loc = Utils.get_stmt_loc s in
        let nbCond = getConditionsNumber e in
        let allConds = getUnitaryConditionsInExp e in
         (* Format.printf "+++++nb = %d@." nbCond; *)
        let finalList = List.append  (genMultiLabels allConds nbCond 0 [] loc) ([s]) in
         (*  Format.printf "+++++10@."; *)
        let b2 = mkBlock finalList in
        let i = mkStmt (Block(b2)) in
          i
      | _ -> s
    in
      match stmt.skind with
      | If _ ->
        (*DoChildren *)
        ChangeDoChildrenPost (stmt, genLabels)
      | _ -> DoChildren
end


(* val generate_labels_prj: Project.t -> unit *)
let generate_labels_prj prj =
  Project.set_current prj;
  Visitor.visitFramacFile
    (new genLabelsVisitor :> Visitor.frama_c_inplace)
    (Ast.get())


let generate_multi_labels_prj prj =
  Project.set_current prj;
  Visitor.visitFramacFile
    (new genMultiLabelsVisitor :> Visitor.frama_c_inplace)
    (Ast.get())

