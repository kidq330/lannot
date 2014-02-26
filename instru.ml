open Cil
open Cil_types
open Lexing

let nextLabelId : int ref = ref 1
let nextMutantId : int ref = ref 0
let operatorCounter : int ref = ref 0
let mutCounter : int ref = ref 0
let foundMutant : int ref = ref 0

(* (file,line, id, cond) *)
let labelsList : (string*int*int*exp) list ref = ref []

(* val print_project: Project.t -> string *)
let print_project prj filename =
  (* Format.printf "+++ %s ++1@." filename; *)
  let out = open_out filename in
  let _ = Format.set_formatter_out_channel out in
  let _ = File.pretty_ast ~prj:prj ~fmt:Format.std_formatter () in
  let _ = close_out out in
  let _ = Format.set_formatter_out_channel Pervasives.stdout in
    filename
    


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
  begin
    match exp.enode with
      | BinOp(LAnd, e1, e2, _) ->
          (getConditionsNumber e1) + (getConditionsNumber e2)
      | BinOp(LOr, e1, e2, _) ->
          (getConditionsNumber e1) + (getConditionsNumber e2)
      | BinOp(_, _e1, _e2, _) -> 1
	  
      | Lval(_lv) -> 1
	  
      | UnOp(LNot, e1, _) -> getConditionsNumber e1
	  
      | _ -> 0
  end



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
	  let labelIdExp = dummy_exp (Const(CInt64(Integer.of_int(!nextLabelId),IInt,None))) in
	  let lineNumber = (fst loc).pos_lnum in
	  let fileName = (fst loc).pos_fname in
	    labelsList := (fileName,lineNumber,!nextLabelId, newCond)::!labelsList;
	    nextLabelId := !nextLabelId+1;
            let stmt = mk_call loc "pc_label" [ newCond; labelIdExp ] in
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
      |	(fileName,lineNb, id, cond)::rest ->
	  let myBuf = Buffer.create 128 in
	  let fmt = Format.formatter_of_buffer myBuf in
	    Printer.pp_exp fmt cond;
	    Format.pp_print_flush fmt ();
	    Buffer.add_string myBuffer ("<label>\n");
	    Buffer.add_string myBuffer ("<id>" ^ (string_of_int id)  ^ "</id>\n");
	    Buffer.add_string myBuffer ("<cond>" ^ (Buffer.contents myBuf) ^ "</cond>\n");
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

let incrementMutantCounters () = 
  (*Format.printf "+++++3@.";*)
  operatorCounter := !operatorCounter + 1;
  if !operatorCounter = 3 then
    begin
      operatorCounter := 0;
    end

(*****************************************)
class genAORMutantsVisitor = object(_self)
  inherit Visitor.frama_c_inplace

  method vexpr e =
    match e.enode with
      | BinOp(Div, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++1@."; *)
	      let op = match !operatorCounter with
		| 0 -> Mult
		| 1 -> PlusA
		| _ -> MinusA
	      in
	      let newEnode = BinOp(op, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren
	
      | BinOp(Mult, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++2@."; *)
	      let op = match !operatorCounter with
		| 0 -> Div
		| 1 -> PlusA
		| _ -> MinusA
	      in
	      let newEnode = BinOp(op, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren


      | BinOp(Mod, _lexp, _rexp, _ty) ->
	  (* Format.printf "+++++3@."; *)
	  Cil.DoChildren

      | BinOp(Shiftlt, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++4@."; *)
	      let newEnode = BinOp(Shiftrt, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		operatorCounter := 2;
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren
	  
      | BinOp(Shiftrt, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++5@."; *)
	      let newEnode = BinOp(Shiftlt, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		operatorCounter := 2;
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren

      | BinOp(PlusA, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++6@."; *)
	      let op = match !operatorCounter with
		| 0 -> Mult
		| 1 -> Div
		| _ -> MinusA
	      in
	      let newEnode = BinOp(op, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren

      | BinOp(MinusA, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++7@."; *)
	      let op = match !operatorCounter with
		| 0 -> Mult
		| 1 -> PlusA
		| _ -> Div
	      in
	      let newEnode = BinOp(op, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren

      | UnOp(Neg, exp, _) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++8@."; *)
	      operatorCounter := 2;
	      incrementMutantCounters ();
	      ChangeDoChildrenPost (exp, fun x -> x)
	    end
	  else
	    DoChildren

      | _ -> 
	  (* Format.printf "+++++9@."; *)
	  Cil.DoChildren

end

(*****************************************)
class genRORMutantsVisitor = object(_self)
  inherit Visitor.frama_c_inplace

  method vexpr e =
    match e.enode with
      | BinOp(Lt, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++1@."; *)
	      let op = match !operatorCounter with
		| 0 -> Le
		| 1 -> Gt
		| _ -> Ge
	      in
	      let newEnode = BinOp(op, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren
	
      | BinOp(Gt, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++2@."; *)
	      let op = match !operatorCounter with
		| 0 -> Lt
		| 1 -> Le
		| _ -> Ge
	      in
	      let newEnode = BinOp(op, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren

      | BinOp(Le, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++4@."; *)
	      let op = match !operatorCounter with
		| 0 -> Lt
		| 1 -> Gt
		| _ -> Ge
	      in
	      let newEnode = BinOp(op, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren
	  
      | BinOp(Ge, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++5@."; *)
	      let op = match !operatorCounter with
		| 0 -> Lt
		| 1 -> Le
		| _ -> Gt
	      in
	      let newEnode = BinOp(op, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren

      | BinOp(Eq, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++6@."; *)
	      let newEnode = BinOp(Ne, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		operatorCounter := 2;
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren

      | BinOp(Ne, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      (* Format.printf "+++++7@."; *)
	      let newEnode = BinOp(Eq, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		operatorCounter := 2;
		incrementMutantCounters ();
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren


      | _ -> 
	  (* Format.printf "+++++9@."; *)
	  Cil.DoChildren

end

(*****************************************)
class genCORMutantsVisitor = object(_self)
  inherit Visitor.frama_c_inplace

  method vexpr e =
    match e.enode with
      | BinOp(LAnd, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      let newEnode = BinOp(LOr, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren

      | BinOp(LOr, lexp, rexp, ty) ->
	  mutCounter := !mutCounter+1;
	  if !mutCounter = !nextMutantId then
	    begin
	      foundMutant := 1;
	      let newEnode = BinOp(LAnd, lexp, rexp, ty) in
	      let newExp = {e with enode = newEnode} in
		ChangeDoChildrenPost (newExp, fun x -> x)
	    end
	  else
	    DoChildren

      | _ -> 
	  (* Format.printf "+++++9@."; *)
	  Cil.DoChildren

end

(*****************************************)
class genABSMutantsVisitor = object(_self)
  inherit Visitor.frama_c_inplace

  method! vstmt_aux stmt =

    let traitLval s e = 
      begin match e.enode with 
	| Lval(l) ->
	    mutCounter := !mutCounter+1;
	    if !mutCounter = !nextMutantId then
	      begin
		foundMutant := 1;
		let loc = Utils.get_stmt_loc s in
		let zeroExp = dummy_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
		let myExp = dummy_exp(BinOp(Lt, e, zeroExp, intType)) in
		let stmt = mk_call loc ~result:l "abs" [e] in
		let finalList = List.append  [(makeLabel myExp loc)] ([stmt;s]) in
		let b2 = mkBlock finalList in
		let i = mkStmt (Block(b2)) in
		  (true,i)
	      end
	    else
	      (false,s)
	| _ -> (false,s)
      end 
    in
    let rec traitBinOp s e =
      begin
	match e.enode with 
	  | BinOp(_, e1, e2, _) ->
	      let (res, sRes) = traitBinOp s e1 in
		if res = true then
		  (true, sRes)
		else
		  begin
		    let (res, sRes) = traitBinOp s e2 in
		      if res = true then
			(true, sRes)
		      else
			(false, s)
		  end

	  | Lval(_) -> 
	      traitLval s e

	  | UnOp (_, e1, _) ->
	      let (res, sRes) = traitBinOp s e1 in
		if res = true then
		  (true, sRes)
		else
		  (false, s)


	  | _ -> (false, s)
      end
    in
      
    let genLabels s =
      match s.skind with
	| Instr(Set(_, e, _)) ->
	    let (_res,sRes) = traitBinOp s e in
	      sRes

	| If(e, _, _, _) ->  
	    begin match e.enode with 
	      | Lval(_) ->
		  let (_res,sRes) = traitLval s e in
		    sRes

	      | BinOp(_, _, _, _) ->
		  let (_res,sRes) = traitBinOp s e in 
		    sRes

	      | UnOp (_, _, _) ->
		  let (_res, sRes) = traitBinOp s e in
		    sRes

	      | _ -> s
	    end

	| _ -> s
    in
      ChangeDoChildrenPost (stmt, genLabels)
     (* match stmt.skind with
	| If _ ->
            
	| _ -> DoChildren*)
	    

end

(*****************************************)


(***  let loc = Cil_datatype.Location.unknown in  
Locations.loc_of_varinfo y
*)
let getLocFromFunction f = 
  match f.sbody.bstmts with
    | a::_ -> Cil_datatype.Stmt.loc a
    | [] -> Cil_datatype.Location.unknown


let makeLabelsFromInput myParam loc =
  match myParam.vtype with 
    | TInt _
    | TFloat _ ->
	(*Format.printf "+++++3@.";*)
	let formalExp = new_exp loc (Lval (var myParam)) in
	let zeroExp = dummy_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
	let exp1 = dummy_exp(BinOp(Lt, formalExp, zeroExp, intType)) in
	let exp2 = dummy_exp(BinOp(Gt, formalExp, zeroExp, intType)) in
	let exp3 = dummy_exp(BinOp(Eq, formalExp, zeroExp, intType)) in
	let stmt1 = makeLabel exp1 loc in
	let stmt2 = makeLabel exp2 loc in
	let stmt3 = makeLabel exp3 loc in
	  [stmt1; stmt2; stmt3]

    | TPtr _ -> 
	let formalExp = new_exp loc (Lval (var myParam)) in
	let zeroExp = dummy_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
	let exp1 = dummy_exp(BinOp(Eq, formalExp, zeroExp, intType)) in
	let exp2 = dummy_exp(BinOp(Ne, formalExp, zeroExp, intType)) in
	let stmt1 = makeLabel exp1 loc in
	let stmt2 = makeLabel exp2 loc in
	  [stmt1; stmt2]
    | _ ->  []

class genPartitionLabelsVisitor = object(_self)
  inherit Visitor.frama_c_inplace

  method! vfunc f =
    (* Format.printf "+++++1@.";*)
    let loc = getLocFromFunction f in
    let rec labelsFromFormals formals = 
      match formals with 
	| [] -> []
	| a :: tail -> 
	    (* Format.printf "+++++2@.";*)
	    List.append (makeLabelsFromInput a loc) (labelsFromFormals tail)
    in 
    let oldBody = mkStmt (Block(f.sbody)) in
    let newStmts = List.append (labelsFromFormals f.sformals) [oldBody] in
    let newBody = mkBlock newStmts in
      f.sbody <- newBody;
      SkipChildren

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


let rec generate_aor_mutants mainProj = 
  (*Format.printf "+++++1@.";*)
  if !operatorCounter = 0 then
    begin
      (*Format.printf "+++++2@.";*)
      nextMutantId := !nextMutantId + 1;
    end;
  let filename = (Project.get_name mainProj) ^ "_aor_m" ^ (string_of_int !nextMutantId) ^ "_" ^ (string_of_int !operatorCounter) ^ ".c" in
    foundMutant := 0;
    mutCounter := 0;
    Project.set_current mainProj;
    let prj_name = "aor_m" ^ (string_of_int !nextMutantId) ^ "_" ^ (string_of_int !operatorCounter) in
    let newProj = File.create_project_from_visitor prj_name (fun prj -> new Visitor.frama_c_copy prj) in 
      Project.set_current newProj;
      Visitor.visitFramacFile
	(new genAORMutantsVisitor :> Visitor.frama_c_inplace)
	(Ast.get());
      let _ = print_project newProj filename in
      if !foundMutant = 1 then
	begin
	  generate_aor_mutants mainProj
	end
      else
	begin
	  ()
	end


let rec generate_ror_mutants mainProj = 
  if !operatorCounter = 0 then
    begin
      nextMutantId := !nextMutantId + 1;
    end;
  let filename = (Project.get_name mainProj) ^ "_ror_m" ^ (string_of_int !nextMutantId) ^ "_" ^ (string_of_int !operatorCounter) ^ ".c" in
    foundMutant := 0;
    mutCounter := 0;
    Project.set_current mainProj;
    let prj_name = "ror_m" ^ (string_of_int !nextMutantId) ^ "_" ^ (string_of_int !operatorCounter) in
    let newProj = File.create_project_from_visitor prj_name (fun prj -> new Visitor.frama_c_copy prj) in 
      Project.set_current newProj;
      Visitor.visitFramacFile
	(new genRORMutantsVisitor :> Visitor.frama_c_inplace)
	(Ast.get());
      let _ = print_project newProj filename in
      if !foundMutant = 1 then
	begin
	  generate_ror_mutants mainProj
	end
      else
	begin
	  ()
	end

 
let rec generate_cor_mutants mainProj = 
  nextMutantId := !nextMutantId + 1;
  let filename = (Project.get_name mainProj) ^ "_cor_m" ^ (string_of_int !nextMutantId) ^ ".c" in
    foundMutant := 0;
    mutCounter := 0;
    Project.set_current mainProj;
    let prj_name = "cor_m" ^ (string_of_int !nextMutantId) in
    let newProj = File.create_project_from_visitor prj_name (fun prj -> new Visitor.frama_c_copy prj) in 
      Project.set_current newProj;
      Visitor.visitFramacFile
	(new genCORMutantsVisitor :> Visitor.frama_c_inplace)
	(Ast.get());
      let _ = print_project newProj filename in
      if !foundMutant = 1 then
	begin
	  generate_cor_mutants mainProj
	end
      else
	begin
	  ()
	end

let rec generate_abs_mutants mainProj = 
  nextMutantId := !nextMutantId + 1;
  let filename = (Project.get_name mainProj) ^ "_abs_m" ^ (string_of_int !nextMutantId) ^ ".c" in
    foundMutant := 0;
    mutCounter := 0;
    Project.set_current mainProj;
    let prj_name = "abs_m" ^ (string_of_int !nextMutantId) in
    let newProj = File.create_project_from_visitor prj_name (fun prj -> new Visitor.frama_c_copy prj) in 
      Project.set_current newProj;
      Visitor.visitFramacFile
	(new genABSMutantsVisitor :> Visitor.frama_c_inplace)
	(Ast.get());
      let _ = print_project newProj filename in
      if !foundMutant = 1 then
	begin
	  generate_abs_mutants mainProj
	end
      else
	begin
	  ()
	end
 

let generate_partition_labels_prj prj =
  Project.set_current prj;
  Visitor.visitFramacFile
    (new genPartitionLabelsVisitor :> Visitor.frama_c_inplace)
    (Ast.get())
