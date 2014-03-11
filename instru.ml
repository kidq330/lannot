open Cil
open Cil_types
open Lexing

let aorOption : bool ref = ref false
let rorOption : bool ref = ref false
let corOption : bool ref = ref false
let absOption : bool ref = ref false

(*
(* val print_project: Project.t -> string *)
let print_project prj filename =
  let out = open_out filename in
  let _ = Format.set_formatter_out_channel out in
  let _ = File.pretty_ast ~prj:prj ~fmt:Format.std_formatter () in
  let _ = close_out out in
  let _ = Format.set_formatter_out_channel Pervasives.stdout in
    filename

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
*)
(*
let makeLabel cond loc ltype = 
  let labelTypeExp = Utils.mk_exp (Const(CStr(ltype))) in
  let labelIdExp = Utils.mk_exp (Const(CInt64(Integer.of_int(!nextLabelId),IInt,None))) in
  let lineNumber = (fst loc).pos_lnum in
  let fileName = (fst loc).pos_fname in
    labelsList := (fileName,lineNumber,!nextLabelId, cond, ltype)::!labelsList;
    nextLabelId := !nextLabelId+1;
    Utils.mk_call "pc_label" [ cond; labelIdExp; labelTypeExp ]
*)

module CC = Annotators.Register (struct
  let name = "CC"
  let descr = "Condition Coverage"

let rec genLabelPerExp mk_label exp loc =
  match exp.enode with
    | BinOp(LAnd, e1, e2, _) ->
        List.append (genLabelPerExp mk_label e1 loc) (genLabelPerExp mk_label e2 loc)

    | BinOp(LOr, e1, e2, _) ->
        List.append (genLabelPerExp mk_label e1 loc) (genLabelPerExp mk_label e2 loc)

    | BinOp(Lt, e1, e2, t) ->
        let nonExp = Utils.mk_exp(BinOp(Ge, e1, e2, t)) in
          [mk_label exp loc; mk_label nonExp loc]

    | BinOp(Gt, e1, e2, t) ->
        let nonExp = Utils.mk_exp(BinOp(Le, e1, e2, t)) in
	  [mk_label exp loc; mk_label nonExp loc]

    | BinOp(Le, e1, e2, t) ->
        let nonExp = Utils.mk_exp(BinOp(Gt, e1, e2, t)) in
	  [mk_label exp loc; mk_label nonExp loc]


    | BinOp(Ge, e1, e2, t) ->
        let nonExp = Utils.mk_exp(BinOp(Lt, e1, e2, t)) in
	  [mk_label exp loc; mk_label nonExp loc]

    | BinOp(Eq, e1, e2, t) ->
        let nonExp = Utils.mk_exp(BinOp(Ne, e1, e2, t)) in
	  [mk_label exp loc; mk_label nonExp loc]

    | BinOp(Ne, e1, e2, t) ->
        let nonExp = Utils.mk_exp(BinOp(Eq, e1, e2, t)) in
	  [mk_label exp loc; mk_label nonExp loc]

    | BinOp(_, _e1, _e2, _) ->
        let nonExp = Utils.mk_exp(UnOp(LNot, exp, intType)) in
	  [mk_label exp loc; mk_label nonExp loc]

    | Lval(_lv) ->
        let nonExp = Utils.mk_exp(UnOp(LNot, exp, intType)) in
	  [mk_label exp loc; mk_label nonExp loc]

    | UnOp(LNot, e1, _) ->
        genLabelPerExp mk_label e1 loc

    | _ -> []


class genLabelsVisitor mk_label = object(_self)
  inherit Visitor.frama_c_inplace

  method! vfunc dec =
    if Annotators.shouldInstrument dec.svar then DoChildren else SkipChildren

  method! vstmt_aux stmt =
    let genLabels s =
      match s.skind with

        | If(e, _, _, _) ->
            let loc = Utils.get_stmt_loc s in
            let finalList = List.append  (genLabelPerExp mk_label e loc) ([s]) in
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

let compute mk_label ast =
  Visitor.visitFramacFileSameGlobals
    (new genLabelsVisitor mk_label :> Visitor.frama_c_inplace)
    ast

end)

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

module MCC = Annotators.Register (struct

  let name = "MCC"
  let descr = "Multiple Condition Coverage"

  let generateStatementFromConditionsList mk_label conds loc =
    let rec mergeConds condsList =
      match condsList with
      | [] -> assert false
      | a::[] -> a
      | a::b::tail->
          let newExp = Utils.mk_exp(BinOp(LAnd, a, b, intType)) in
            mergeConds (List.append [newExp] tail)
    in
      match conds with
      | [] ->
            let stmt = mk_label (Cil.one Cil_datatype.Location.unknown) loc in
              stmt
      | _ ->
          let newCond = mergeConds conds in
          let stmt = mk_label newCond loc in
	  stmt

  let getNegativeCond exp =
    match exp.enode with
    | BinOp(Lt, e1, e2, t) ->
        Utils.mk_exp(BinOp(Ge, e1, e2, t))

    | BinOp(Gt, e1, e2, t) ->
        Utils.mk_exp(BinOp(Le, e1, e2, t))

    | BinOp(Le, e1, e2, t) ->
        Utils.mk_exp(BinOp(Gt, e1, e2, t))

    | BinOp(Ge, e1, e2, t) ->
        Utils.mk_exp(BinOp(Lt, e1, e2, t))

    | BinOp(Eq, e1, e2, t) ->
        Utils.mk_exp(BinOp(Ne, e1, e2, t))

    | BinOp(Ne, e1, e2, t) ->
        Utils.mk_exp(BinOp(Eq, e1, e2, t))

    | Lval(_lv) ->
        Utils.mk_exp(UnOp(LNot, exp, intType))

    | UnOp(LNot, e1, _) -> e1

    | _ -> Utils.mk_exp(UnOp(LNot, exp, Cil.intType))

  let rec genMultiLabel mk_label allConds conditionsNb currentNb newConds cumul loc =
    match allConds with
    | [] ->
        generateStatementFromConditionsList mk_label newConds loc

    | a :: tail ->
        (* let ex = pow(2, conditionsNb) in *)
        (* Format.printf "+++++5-3- %d@." ex; *)
        if (currentNb - cumul) / ( pow(2, conditionsNb)) = 1 then
          begin
            let curCond = a in
            let newCumul = cumul + ( pow(2, conditionsNb) ) in
            let newNewConds = List.append newConds [curCond] in
              genMultiLabel mk_label tail (conditionsNb-1) currentNb newNewConds newCumul loc
          end
        else
          begin
            let curCond = getNegativeCond a in
            let newCumul = cumul in
            let newNewConds = List.append newConds [curCond] in
              genMultiLabel mk_label tail (conditionsNb-1) currentNb newNewConds newCumul loc
          end

  let rec genMultiLabels mk_label allConds conditionsNb currentNb labelStmts loc =
    let casesNb = pow(2, conditionsNb) in
    if currentNb = casesNb then
      begin
        labelStmts
      end
    else
      begin
        let curStmt = genMultiLabel mk_label allConds (conditionsNb-1) currentNb [] 0 loc in
          let newStmts = List.append labelStmts [curStmt] in
            genMultiLabels mk_label allConds conditionsNb (currentNb+1) newStmts loc
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


  class genMultiLabelsVisitor mk_label = object(_self)
    inherit Visitor.frama_c_inplace

    method! vfunc dec =
      if Annotators.shouldInstrument dec.svar then DoChildren else SkipChildren

    method! vstmt_aux stmt =
      let genLabels s =
        match s.skind with
        | If(e, _, _, _) ->
          let loc = Utils.get_stmt_loc s in
          let nbCond = getConditionsNumber e in
          let allConds = getUnitaryConditionsInExp e in
          let finalList = List.append (genMultiLabels mk_label allConds nbCond 0 [] loc) ([s]) in
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

  let compute mk_label ast =
    Visitor.visitFramacFileSameGlobals
      (new genMultiLabelsVisitor mk_label :> Visitor.frama_c_inplace)
      ast
end)

(*****************************************)


(***  let loc = Cil_datatype.Location.unknown in  
Locations.loc_of_varinfo y
*)
let getLocFromFunction f = 
  match f.sbody.bstmts with
    | a::_ -> Cil_datatype.Stmt.loc a
    | [] -> Cil_datatype.Location.unknown

module Partition = Annotators.Register (struct

  let name = "IDP"
  let descr = "Input Domain Partition"

  let makeLabelsFromInput mk_label myParam loc =
    match myParam.vtype with 
    | TInt _
    | TFloat _ ->
	let formalExp = Utils.mk_exp (Lval (var myParam)) in
	let zeroExp = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
	let exp1 = Utils.mk_exp (BinOp(Lt, formalExp, zeroExp, intType)) in
	let exp2 = Utils.mk_exp (BinOp(Gt, formalExp, zeroExp, intType)) in
	let exp3 = Utils.mk_exp (BinOp(Eq, formalExp, zeroExp, intType)) in
	let stmt1 = mk_label exp1 loc in
	let stmt2 = mk_label exp2 loc in
	let stmt3 = mk_label exp3 loc in
	  [stmt1; stmt2; stmt3]
    | TPtr _ ->
	let formalExp = Utils.mk_exp (Lval (var myParam)) in
	let zeroExp = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
	let exp1 = Utils.mk_exp(BinOp(Eq, formalExp, zeroExp, intType)) in
	let exp2 = Utils.mk_exp(BinOp(Ne, formalExp, zeroExp, intType)) in
	let stmt1 = mk_label exp1 loc in
	let stmt2 = mk_label exp2 loc in
	  [stmt1; stmt2]
    | _ ->  []

  class inputDomainVisitor mk_label = object
    inherit Visitor.frama_c_inplace

    method! vfunc f =
      if Annotators.shouldInstrument f.svar then
      begin
	let loc = getLocFromFunction f in
	let rec labelsFromFormals formals = 
	  match formals with 
	    | [] -> []
	    | a :: tail -> 
		List.append (makeLabelsFromInput mk_label a loc) (labelsFromFormals tail)
	in 
	let oldBody = mkStmt (Block(f.sbody)) in
	let newStmts = List.append (labelsFromFormals f.sformals) [oldBody] in
	let newBody = mkBlock newStmts in
	  f.sbody <- newBody;
      end;
      SkipChildren
  end

  let compute mk_label ast =
    Visitor.visitFramacFileSameGlobals
      (new inputDomainVisitor mk_label :> Visitor.frama_c_inplace)
      ast

end)

(*****************************************)

module WM = Annotators.RegisterWithExtraTags (struct

  let name = "WM"
  let descr = "Weak Mutation"

  class mutationVisitor mk_label = object(_self)
    inherit Visitor.frama_c_inplace

    method! vfunc f =
      if Annotators.shouldInstrument f.svar then DoChildren else SkipChildren

  method! vstmt_aux stmt =      	  
    let makeLabel cond loc category = mk_label ~extra:[category] cond loc in
    let rec traitExp e loc = 
      let labelsStmts = ref [] in
	begin match e.enode with
	  | BinOp(LAnd, lexp, rexp, ty) ->
	      if !corOption = true then
		begin
		  let newEnode = BinOp(LOr, lexp, rexp, ty) in
		  let newExp = {e with enode = newEnode} in
		  let labelExp = Utils.mk_exp(BinOp(Ne, newExp, e, ty)) in 
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
		  let labelExp = Utils.mk_exp(BinOp(Ne, newExp, e, ty)) in 
		  let labelStmt = makeLabel labelExp loc "COR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts
		
	  | BinOp(Div, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = Utils.mk_exp(BinOp(Mult, lexp, rexp, ty)) in
		  let newExp2 = Utils.mk_exp(BinOp(PlusA, lexp, rexp, ty)) in
		  let newExp3 = Utils.mk_exp(BinOp(MinusA, lexp, rexp, ty)) in
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, newExp3, e, ty)) in 
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
		  let newExp1 = Utils.mk_exp(BinOp(Div, lexp, rexp, ty)) in
		  let newExp2 = Utils.mk_exp(BinOp(PlusA, lexp, rexp, ty)) in
		  let newExp3 = Utils.mk_exp(BinOp(MinusA, lexp, rexp, ty)) in
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, newExp3, e, ty)) in 
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
		  let newExp1 = Utils.mk_exp(BinOp(Mult, lexp, rexp, ty)) in
		  let newExp2 = Utils.mk_exp(BinOp(Div, lexp, rexp, ty)) in
		  let newExp3 = Utils.mk_exp(BinOp(MinusA, lexp, rexp, ty)) in
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, newExp3, e, ty)) in 
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
		  let newExp1 = Utils.mk_exp(BinOp(Mult, lexp, rexp, ty)) in
		  let newExp2 = Utils.mk_exp(BinOp(Div, lexp, rexp, ty)) in
		  let newExp3 = Utils.mk_exp(BinOp(PlusA, lexp, rexp, ty)) in
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, newExp3, e, ty)) in 
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
		  let newExp1 = Utils.mk_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp2 = Utils.mk_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let newExp3 = Utils.mk_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, newExp3, e, ty)) in 
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
		  let newExp1 = Utils.mk_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp2 = Utils.mk_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp3 = Utils.mk_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, newExp3, e, ty)) in 
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
		  let newExp1 = Utils.mk_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp2 = Utils.mk_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let newExp3 = Utils.mk_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, newExp3, e, ty)) in 
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
		  let newExp1 = Utils.mk_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp2 = Utils.mk_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp3 = Utils.mk_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, newExp1, e, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, newExp2, e, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, newExp3, e, ty)) in 
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
		  let newExp = Utils.mk_exp(BinOp(Ne, lexp, rexp, ty)) in
		  let labelExp = Utils.mk_exp(BinOp(Ne, newExp, e, ty)) in  
		  let labelStmt = makeLabel labelExp loc "ROR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Ne, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
		  let newExp = Utils.mk_exp(BinOp(Eq, lexp, rexp, ty)) in
		  let labelExp = Utils.mk_exp(BinOp(Ne, newExp, e, ty)) in  
		  let labelStmt = makeLabel labelExp loc "ROR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Shiftlt, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp = Utils.mk_exp(BinOp(Shiftrt, lexp, rexp, ty)) in
		  let labelExp = Utils.mk_exp(BinOp(Ne, newExp, e, ty)) in  
		  let labelStmt = makeLabel labelExp loc "AOR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc);
	      !labelsStmts

	  | BinOp(Shiftrt, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp = Utils.mk_exp(BinOp(Shiftlt, lexp, rexp, ty)) in
		  let labelExp = Utils.mk_exp(BinOp(Ne, newExp, e, ty)) in  
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
		  let labelExp = Utils.mk_exp(BinOp(Ne, exp, e, ty)) in  
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
		let zeroExp = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
		let labelExp = Utils.mk_exp(BinOp(Lt, e, zeroExp, intType)) in
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

  let compute mk_label ast =
    Visitor.visitFramacFileSameGlobals
      (new mutationVisitor mk_label :> Visitor.frama_c_inplace)
      ast
end)

