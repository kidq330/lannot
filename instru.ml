open Cil
open Cil_types
open Lexing

(* val mk_call: loc -> lval option -> string -> exp list -> stmt *)
let mk_call loc ?result fname args =
  let new_lval loc v = new_exp loc (Lval (var v)) in
  let t = match result with
    | Some(Var(v),_) -> v.vtype
    | _ -> voidType in
  let ty = TFun(t, None, false, []) in
  let f = new_lval loc (makeGlobalVar fname ty) in
  mkStmt ~valid_sid:true (Instr(Call(result, f, args, loc)))

let rec genLabelPerExp exp loc = 
  match exp.enode with
    | BinOp(LAnd, e1, e2, _) ->
	List.append (genLabelPerExp e1 loc) (genLabelPerExp e2 loc)

    | BinOp(LOr, e1, e2, _) ->
	List.append (genLabelPerExp e1 loc) (genLabelPerExp e2 loc)
	  
    | BinOp(Lt, e1, e2, t) ->
	let nonExp = dummy_exp(BinOp(Ge, e1, e2, t)) in
	  [mk_call loc "pc_label" [ exp ]; mk_call loc "pc_label" [ nonExp ]]
	    
    | BinOp(Gt, e1, e2, t) ->
	let nonExp = dummy_exp(BinOp(Le, e1, e2, t)) in
	  [mk_call loc "pc_label" [ exp ]; mk_call loc "pc_label" [ nonExp ]]
	    
    | BinOp(Le, e1, e2, t) ->
	let nonExp = dummy_exp(BinOp(Gt, e1, e2, t)) in
	  [mk_call loc "pc_label" [ exp ]; mk_call loc "pc_label" [ nonExp ]]
	    
    | BinOp(Ge, e1, e2, t) ->
	let nonExp = dummy_exp(BinOp(Lt, e1, e2, t)) in
	  [mk_call loc "pc_label" [ exp ]; mk_call loc "pc_label" [ nonExp ]]
	    
    | BinOp(Eq, e1, e2, t) ->
	let nonExp = dummy_exp(BinOp(Ne, e1, e2, t)) in
	  [mk_call loc "pc_label" [ exp ]; mk_call loc "pc_label" [ nonExp ]]
	    
    | BinOp(Ne, e1, e2, t) ->
	let nonExp = dummy_exp(BinOp(Eq, e1, e2, t)) in
	  [mk_call loc "pc_label" [ exp ]; mk_call loc "pc_label" [ nonExp ]]
	    
    | BinOp(_, _e1, _e2, _) ->
	let nonExp = dummy_exp(UnOp(LNot, exp, intType)) in
	  [mk_call loc "pc_label" [ exp ]; mk_call loc "pc_label" [ nonExp ]] 

    | Lval(_lv) ->
	let nonExp = dummy_exp(UnOp(LNot, exp, intType)) in
	  [mk_call loc "pc_label" [ exp ]; mk_call loc "pc_label" [ nonExp ]] 
	    
    | UnOp(LNot, e1, _) ->
	genLabelPerExp e1 loc
	  
    | _ -> []


class genLabelsVisitor = object(self)
  inherit Visitor.frama_c_inplace
  
  method vstmt_aux stmt =
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


class genMultiLabelsVisitor = object(self)
  inherit Visitor.frama_c_inplace
  
  method vstmt_aux stmt =
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
    
