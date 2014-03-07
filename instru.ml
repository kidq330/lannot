open Cil
open Cil_types
open Lexing

let nextLabelId : int ref = ref 1

let multiCondOption : bool ref = ref false
let aorOption : bool ref = ref false
let rorOption : bool ref = ref false
let corOption : bool ref = ref false
let absOption : bool ref = ref false
let partitionOption : bool ref = ref false
let simpleOption : bool ref = ref false

(* (file,line, id, cond, type) *)
let labelsList : (string*int*int*exp*string) list ref = ref []

(* val print_project: Project.t -> string *)
let print_project prj filename =
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

let makeLabel cond loc ltype = 
  let labelTypeExp = dummy_exp (Const(CStr(ltype))) in
  let labelIdExp = dummy_exp (Const(CInt64(Integer.of_int(!nextLabelId),IInt,None))) in
  let lineNumber = (fst loc).pos_lnum in
  let fileName = (fst loc).pos_fname in
    labelsList := (fileName,lineNumber,!nextLabelId, cond, ltype)::!labelsList;
    nextLabelId := !nextLabelId+1;
    mk_call loc "pc_label" [ cond; labelIdExp; labelTypeExp ]

let rec genLabelPerExp exp loc =
  match exp.enode with
    | BinOp(LAnd, e1, e2, _) ->
        List.append (genLabelPerExp e1 loc) (genLabelPerExp e2 loc)

    | BinOp(LOr, e1, e2, _) ->
        List.append (genLabelPerExp e1 loc) (genLabelPerExp e2 loc)

    | BinOp(Lt, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Ge, e1, e2, t)) in
          [makeLabel exp loc "CC"; makeLabel nonExp loc "CC"]

    | BinOp(Gt, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Le, e1, e2, t)) in
	  [makeLabel exp loc "CC"; makeLabel nonExp loc "CC"]

    | BinOp(Le, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Gt, e1, e2, t)) in
	  [makeLabel exp loc "CC"; makeLabel nonExp loc "CC"]


    | BinOp(Ge, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Lt, e1, e2, t)) in
	  [makeLabel exp loc "CC"; makeLabel nonExp loc "CC"]

    | BinOp(Eq, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Ne, e1, e2, t)) in
	  [makeLabel exp loc "CC"; makeLabel nonExp loc "CC"]

    | BinOp(Ne, e1, e2, t) ->
        let nonExp = dummy_exp(BinOp(Eq, e1, e2, t)) in
	  [makeLabel exp loc "CC"; makeLabel nonExp loc "CC"]

    | BinOp(_, _e1, _e2, _) ->
        let nonExp = dummy_exp(UnOp(LNot, exp, intType)) in
	  [makeLabel exp loc "CC"; makeLabel nonExp loc "CC"]

    | Lval(_lv) ->
        let nonExp = dummy_exp(UnOp(LNot, exp, intType)) in
	  [makeLabel exp loc "CC"; makeLabel nonExp loc "CC"]

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
	  let lineNumber = (fst loc).pos_lnum in
	  let fileName = (fst loc).pos_fname in
	    labelsList := (fileName,lineNumber,!nextLabelId, newCond, "MCC")::!labelsList;
            let stmt = makeLabel newCond loc "MCC" in
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
      |	(fileName,lineNb, id, cond, ltype)::rest ->
	  let myBuf = Buffer.create 128 in
	  let fmt = Format.formatter_of_buffer myBuf in
	    Printer.pp_exp fmt cond;
	    Format.pp_print_flush fmt ();
	    Buffer.add_string myBuffer ("<label>\n");
	    Buffer.add_string myBuffer ("<id>" ^ (string_of_int id)  ^ "</id>\n");
	    Buffer.add_string myBuffer ("<cond>" ^ (Buffer.contents myBuf) ^ "</cond>\n");
	    Buffer.add_string myBuffer ("<file>" ^ fileName  ^ "</file>\n");
	    Buffer.add_string myBuffer ("<line>" ^ (string_of_int lineNb)  ^ "</line>\n");
	    Buffer.add_string myBuffer ("<type>" ^ ltype  ^ "</type>\n");
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
        let finalList = List.append  (genMultiLabels allConds nbCond 0 [] loc) ([s]) in
        let b2 = mkBlock finalList in
        let i = mkStmt (Block(b2)) in
          i
      | _ -> s
    in
      match stmt.skind with
      | If _ ->
        ChangeDoChildrenPost (stmt, genLabels)
      | _ -> DoChildren
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
	let formalExp = new_exp loc (Lval (var myParam)) in
	let zeroExp = dummy_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
	let exp1 = dummy_exp(BinOp(Lt, formalExp, zeroExp, intType)) in
	let exp2 = dummy_exp(BinOp(Gt, formalExp, zeroExp, intType)) in
	let exp3 = dummy_exp(BinOp(Eq, formalExp, zeroExp, intType)) in
	let stmt1 = makeLabel exp1 loc "PARTITION" in
	let stmt2 = makeLabel exp2 loc "PARTITION" in
	let stmt3 = makeLabel exp3 loc "PARTITION" in
	  [stmt1; stmt2; stmt3]

    | TPtr _ -> 
	let formalExp = new_exp loc (Lval (var myParam)) in
	let zeroExp = dummy_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
	let exp1 = dummy_exp(BinOp(Eq, formalExp, zeroExp, intType)) in
	let exp2 = dummy_exp(BinOp(Ne, formalExp, zeroExp, intType)) in
	let stmt1 = makeLabel exp1 loc "PARTITION" in
	let stmt2 = makeLabel exp2 loc "PARTITION" in
	  [stmt1; stmt2]
    | _ ->  []


(*****************************************)
(*****************************************)
class labelsVisitor = object(_self)
  inherit Visitor.frama_c_inplace
    
  method! vfunc f =
    if !partitionOption = true then
      begin
	let loc = getLocFromFunction f in
	let rec labelsFromFormals formals = 
	  match formals with 
	    | [] -> []
	    | a :: tail -> 
		List.append (makeLabelsFromInput a loc) (labelsFromFormals tail)
	in 
	let oldBody = mkStmt (Block(f.sbody)) in
	let newStmts = List.append (labelsFromFormals f.sformals) [oldBody] in
	let newBody = mkBlock newStmts in
	  f.sbody <- newBody;
      end;
    DoChildren

  method! vstmt_aux stmt =      	  
    let rec traitExp e loc = 
      let labelsStmts = ref [] in
	begin match e.enode with
	  | BinOp(LAnd, lexp, rexp, ty) ->
	      if !corOption = true then
		begin
		  let newEnode = BinOp(LOr, lexp, rexp, ty) in
		  let newExp = {e with enode = newEnode} in
		  let labelExp = dummy_exp(BinOp(Ne, newExp, e, ty)) in 
		  let labelStmt = makeLabel labelExp loc "COR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts
	     
		
	  | BinOp(LOr, lexp, rexp, ty) ->
	      if !corOption = true then
		begin
		  let newEnode = BinOp(LAnd, lexp, rexp, ty) in
		  let newExp = {e with enode = newEnode} in
		  let labelExp = dummy_exp(BinOp(Ne, newExp, e, ty)) in 
		  let labelStmt = makeLabel labelExp loc "COR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts
		
	  | BinOp(Div, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = dummy_exp(BinOp(Mult, lexp, rexp, ty)) in
		  let newExp2 = dummy_exp(BinOp(PlusA, lexp, rexp, ty)) in
		  let newExp3 = dummy_exp(BinOp(MinusA, lexp, rexp, ty)) in
		  let labelExp1 = dummy_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = dummy_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = dummy_exp(BinOp(Ne, newExp3, e, ty)) in 
		  let labelStmt1 = makeLabel labelExp1 loc "AOR" in
		  let labelStmt2 = makeLabel labelExp2 loc "AOR" in
		  let labelStmt3 = makeLabel labelExp3 loc "AOR" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Mult, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = dummy_exp(BinOp(Div, lexp, rexp, ty)) in
		  let newExp2 = dummy_exp(BinOp(PlusA, lexp, rexp, ty)) in
		  let newExp3 = dummy_exp(BinOp(MinusA, lexp, rexp, ty)) in
		  let labelExp1 = dummy_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = dummy_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = dummy_exp(BinOp(Ne, newExp3, e, ty)) in 
		  let labelStmt1 = makeLabel labelExp1 loc "AOR" in
		  let labelStmt2 = makeLabel labelExp2 loc "AOR" in
		  let labelStmt3 = makeLabel labelExp3 loc "AOR" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(PlusA, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = dummy_exp(BinOp(Mult, lexp, rexp, ty)) in
		  let newExp2 = dummy_exp(BinOp(Div, lexp, rexp, ty)) in
		  let newExp3 = dummy_exp(BinOp(MinusA, lexp, rexp, ty)) in
		  let labelExp1 = dummy_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = dummy_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = dummy_exp(BinOp(Ne, newExp3, e, ty)) in 
		  let labelStmt1 = makeLabel labelExp1 loc "AOR" in
		  let labelStmt2 = makeLabel labelExp2 loc "AOR" in
		  let labelStmt3 = makeLabel labelExp3 loc "AOR" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(MinusA, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = dummy_exp(BinOp(Mult, lexp, rexp, ty)) in
		  let newExp2 = dummy_exp(BinOp(Div, lexp, rexp, ty)) in
		  let newExp3 = dummy_exp(BinOp(PlusA, lexp, rexp, ty)) in
		  let labelExp1 = dummy_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = dummy_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = dummy_exp(BinOp(Ne, newExp3, e, ty)) in 
		  let labelStmt1 = makeLabel labelExp1 loc "AOR" in
		  let labelStmt2 = makeLabel labelExp2 loc "AOR" in
		  let labelStmt3 = makeLabel labelExp3 loc "AOR" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Lt, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
		  let newExp1 = dummy_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp2 = dummy_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let newExp3 = dummy_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let labelExp1 = dummy_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = dummy_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = dummy_exp(BinOp(Ne, newExp3, e, ty)) in 
		  let labelStmt1 = makeLabel labelExp1 loc "ROR" in
		  let labelStmt2 = makeLabel labelExp2 loc "ROR" in
		  let labelStmt3 = makeLabel labelExp3 loc "ROR" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Gt, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
		  let newExp1 = dummy_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp2 = dummy_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp3 = dummy_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let labelExp1 = dummy_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = dummy_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = dummy_exp(BinOp(Ne, newExp3, e, ty)) in 
		  let labelStmt1 = makeLabel labelExp1 loc "ROR" in
		  let labelStmt2 = makeLabel labelExp2 loc "ROR" in
		  let labelStmt3 = makeLabel labelExp3 loc "ROR" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Le, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
		  let newExp1 = dummy_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp2 = dummy_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let newExp3 = dummy_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let labelExp1 = dummy_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = dummy_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = dummy_exp(BinOp(Ne, newExp3, e, ty)) in 
		  let labelStmt1 = makeLabel labelExp1 loc "ROR" in
		  let labelStmt2 = makeLabel labelExp2 loc "ROR" in
		  let labelStmt3 = makeLabel labelExp3 loc "ROR" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Ge, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
		  let newExp1 = dummy_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp2 = dummy_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp3 = dummy_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let labelExp1 = dummy_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = dummy_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = dummy_exp(BinOp(Ne, newExp3, e, ty)) in 
		  let labelStmt1 = makeLabel labelExp1 loc "ROR" in
		  let labelStmt2 = makeLabel labelExp2 loc "ROR" in
		  let labelStmt3 = makeLabel labelExp3 loc "ROR" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Eq, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
		  let newExp = dummy_exp(BinOp(Ne, lexp, rexp, ty)) in
		  let labelExp = dummy_exp(BinOp(Ne, newExp, e, ty)) in  
		  let labelStmt = makeLabel labelExp loc "ROR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Ne, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
		  let newExp = dummy_exp(BinOp(Eq, lexp, rexp, ty)) in
		  let labelExp = dummy_exp(BinOp(Ne, newExp, e, ty)) in  
		  let labelStmt = makeLabel labelExp loc "ROR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Shiftlt, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp = dummy_exp(BinOp(Shiftrt, lexp, rexp, ty)) in
		  let labelExp = dummy_exp(BinOp(Ne, newExp, e, ty)) in  
		  let labelStmt = makeLabel labelExp loc "AOR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Shiftrt, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp = dummy_exp(BinOp(Shiftlt, lexp, rexp, ty)) in
		  let labelExp = dummy_exp(BinOp(Ne, newExp, e, ty)) in  
		  let labelStmt = makeLabel labelExp loc "AOR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts


	  | BinOp(_op, lexp, rexp, _ty) ->
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts
	
	  | UnOp(Neg, exp, ty) ->
	      if !aorOption = true then
		begin
		  let labelExp = dummy_exp(BinOp(Ne, exp, e, ty)) in  
		  let labelStmt = makeLabel labelExp loc "AOR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp exp loc);
	      !labelsStmts
	
	  | UnOp(_op, exp, _ty)->
	      traitExp exp loc

	  | Lval(_l) ->
	    if !absOption = true then
	      begin
		let zeroExp = dummy_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
		let labelExp = dummy_exp(BinOp(Lt, e, zeroExp, intType)) in
		let labelStmt = makeLabel labelExp loc "ABS" in
		  labelsStmts := List.append !labelsStmts [labelStmt];
	      end;
	      !labelsStmts
		
	  | _ -> []
	end
    in
    let traitStmt s e loc = 
      let labelsList = traitExp e loc in
	match labelsList with 
	  | [] -> s
	  | _ ->
	      let finalList = List.append  labelsList [s] in
	      let b2 = mkBlock finalList in
	      let i = mkStmt (Block(b2)) in	  
		i
    in
    let genLabels s =
      match s.skind with
	| Instr(Set(_l, e, loc)) ->
	    traitStmt s e loc
		
	| If(e, _l, _ll, loc) ->
	    traitStmt s e loc
	      
	| Return(e, loc) ->
	    begin match e with 
	      | Some exp ->
		  traitStmt s exp loc
	      | _ -> s 
	    end
	    
	      
	| Switch(e, _l,_ll, loc) ->
	    traitStmt s e loc
	      
      | _ -> s
    in  
      ChangeDoChildrenPost (stmt, genLabels)

end


let getProject _stmts prj_name =
  let prj = File.create_project_from_visitor prj_name (fun prj -> new Visitor.frama_c_copy prj) in 
    prj


(*****************************************)
(* val generate_labels_prj: Project.t -> unit *)
let generate_wm_prj mainProj =
  Project.set_current mainProj;
  Visitor.visitFramacFile
    (new labelsVisitor :> Visitor.frama_c_inplace)
    (Ast.get())


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


module type Type = sig
  val process : stmt list -> unit
  val name : string
end

(*************************)
(* NONE                  *)
(*************************)
module CC:Type = struct
  let name = "none"
  let process _ =
    let prj = getProject !Utils.all_stmts (Config.input_file()) in
    let filename = (Project.get_name prj) ^ "_CC.c" in
      generate_labels_prj prj;
      let _ = print_project prj filename in
	()
end

module MCC:Type = struct
  let name = "multi"
  let process _ =
    let mainProj = Project.current () in
    let prj = getProject !Utils.all_stmts (Config.input_file()) in
    let filename = (Project.get_name prj) ^ "_MCC.c" in
      generate_multi_labels_prj prj;
      let _ = print_project prj filename in
	Project.set_current mainProj;
	()
end


module WM:Type = struct
  let name = "label"
  let process _ =
    let mainProj = Project.current () in
    let prj = getProject !Utils.all_stmts (Config.input_file()) in
    let filename = (Project.get_name prj) ^ "_WM.c" in
      generate_wm_prj prj;
      let _ = print_project prj filename in
	Project.set_current mainProj;
	()
end
