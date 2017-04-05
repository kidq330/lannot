(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  You may redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful, but WITHOUT     *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General      *)
(*  Public License for more details.                                      *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1 for more        *)
(*  details (enclosed in the file LICENSE).                               *)
(*                                                                        *)
(**************************************************************************)

open Cil
open Cil_types

let aorOption : bool ref = ref true
let rorOption : bool ref = ref true
let corOption : bool ref = ref true
let absOption : bool ref = ref true
let btworOption : bool ref = ref false
let shiftOption : bool ref = ref false
let pmOption : bool ref = ref false
let crcOption : bool ref = ref false
let vrrLOption : bool ref = ref false
let vrrGOption : bool ref = ref false
let uoiOption : bool ref = ref true
let sdlOption : bool ref = ref false

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
  let help = "Condition Coverage"

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
  let help = "Multiple Condition Coverage"

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
  let help = "Input Domain Partition"

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



let get_integer_from_const const = match const.enode with (Const (CInt64 (id, IInt, None))) -> (Integer.to_int id)
                              | _ -> failwith "Unexpected pattern matching result"

let get_label_id label_stmt = match label_stmt.skind with (Instr (Call (_, _, args, _))) -> (get_integer_from_const (List.nth args 1))
                              | _ -> failwith "Unexpected pattern matching result"

module Labels = Map.Make(struct type t = int let compare (x:int) y = if x < y then -1 else if x > y then 1 else 0 end)

let mylabels = ref Labels.empty 

module SM = Annotators.RegisterStrong (struct

  let name = "SM"
  let help = "Strong Mutation"

(*
class mutators mut = object(_self)
    inherit Visitor.frama_c_inplace
  method vexpr exp = ChangeTo mut
end
*)

  class strongmutationVisitor mk_label = object(_self)
    inherit Visitor.frama_c_inplace

   val mutable localVars = ref []

    method! vfunc f =
      if Annotators.shouldInstrument f.svar then begin (* let fa data = (Options.feedback "%s varinfo %s" f.svar.vname data.vname) in List.iter fa f.slocals; *) DoChildren end else SkipChildren


method! vblock bloc = localVars :=  bloc.blocals; (*
let mm vi ii = Options.feedback "Globals %s" vi.vname in Globals.Vars.iter mm;
let fa data = (Options.feedback "varinfo %s" data.vname) in List.iter fa bloc.blocals; *) DoChildren 


  method! vstmt_aux stmt =
    let makeLabel ?(origstmt=Cil.invalidStmt) ?(mutstmt=Cil.invalidStmt) statement cond mut label loc category = mk_label ~extra:[category] statement cond mut origstmt mutstmt label loc in
    let rec traitExp e loc wholeexp = 
      let rec makemutexp whole origexp mutexp =
	  if whole.eid = origexp.eid then begin mutexp end else begin
 match whole.enode with
          | BinOp(op, lexp, rexp, ty) -> let newEnode = BinOp(op, (makemutexp lexp origexp mutexp), (makemutexp rexp origexp mutexp), ty) in let newExp = {e with enode = newEnode} in newExp
          | UnOp(op, exp, ty)-> let newEnode = UnOp(op, (makemutexp exp origexp mutexp), ty) in let newExp = {e with enode = newEnode} in newExp
          | CastE(ty, exp) ->  let newEnode = CastE(ty, (makemutexp exp origexp mutexp)) in let newExp = {e with enode = newEnode} in newExp
          | _ -> whole
end in 
(*          | Lval(l) -> *)
(*	  | _ -> mutexp;;*)
      let labelsStmts = ref [] in
	begin match e.enode with
	  | BinOp(LAnd, lexp, rexp, ty) ->
	      if !corOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
		  let newExp4 = Utils.mk_exp(BinOp(Eq, lexp, rexp, ty)) in (* {e with enode = BinOp(Eq, lexp, rexp, ty)} in *)

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  
                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "COR-LAND-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "COR-LAND-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "COR-LAND-3" in
		  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "COR-LAND-4" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

		
	  | BinOp(LOr, lexp, rexp, ty) ->
	      if !corOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in
		  let newExp4 = Utils.mk_exp(BinOp(Ne, lexp, rexp, ty)) in (*{e with enode = BinOp(Ne, lexp, rexp, ty)} in*)

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  
                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "COR-LOR-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "COR-LOR-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "COR-LOR-3" in
		  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "COR-LOR-4" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts
		
	  | BinOp(BOr, lexp, rexp, ty) ->
	      if !btworOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(BAnd, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(BXor, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  
                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "BTW-BOR-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "BTW-BOR-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "BTW-BOR-3" in
		  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "BTW-BOR-4" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(BAnd, lexp, rexp, ty) ->
	      if !btworOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(BOr, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(BXor, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  
                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "BTW-BAnd-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "BTW-BAnd-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "BTW-BAnd-3" in
		  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "BTW-BAnd-4" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(BXor, lexp, rexp, ty) ->
	      if !btworOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(BOr, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(BAnd, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  
                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "BXor-BAnd-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "BXor-BAnd-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "BXor-BAnd-3" in
		  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "BXor-BAnd-4" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(Shiftlt, lexp, rexp, ty) ->
	      if !shiftOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Shiftrt, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  
                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "Shift-Shiftlt-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "Shift-Shiftlt-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "Shift-Shiftlt-3" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(Shiftrt, lexp, rexp, ty) ->
	      if !shiftOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Shiftlt, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  
                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "Shift-Shiftrt-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "Shift-Shiftrt-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "Shift-Shiftrt-3" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(Div, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Mult, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(Mod, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 

		  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "AOR-Div-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "AOR-Div-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "AOR-Div-3" in
                  let typ = Cil.typeOf e in
                  if ( not (Cil.isFloatingType typ) ) then begin
                       let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "AOR-Div-4" in
                       labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
                  end
                  else begin
                     labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
                  end
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(Mult, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(PlusA, lexp, rexp, ty)) in
		  let checkExp1 = Utils.mk_exp(BinOp(Div, lexp, rexp, ty)) in                  
                  let checkExp2 = Utils.mk_exp(BinOp(LAnd, rexp, checkExp1, ty)) in 
                  let newExp4 = Utils.mk_exp(BinOp(LOr, checkExp2, Utils.mk_exp(UnOp(LNot, rexp, ty)), ty)) in
		  
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 

		  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "AOR-Mult-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "AOR-Mult-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "AOR-Mult-3" in
		  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "AOR-Mult-4" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(PlusA, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(MinusA, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(Mult, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 

		  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "AOR-PlusA-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "AOR-PlusA-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "AOR-PlusA-3" in
		  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "AOR-PlusA-4" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(PlusPI, lexp, rexp, ty) ->
	      if !pmOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(MinusPI, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 		  
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 

		  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "PM-PPI-11" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "PM-PPI-13" in

                  (*let ff = ref false in
                  if Utils.isPointerRemoveSafe wholeexp e ff then begin 
                     let newExp2 = Utils.mk_exp(rexp.enode) in
                     let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
                     let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "PM-PPI-12" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
                  end
                  else*) labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(MinusA, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(PlusA, lexp, rexp, ty)) in
		  let checkExp1 = Utils.mk_exp(BinOp(Div, lexp, rexp, ty)) in                  
                  let checkExp2 = Utils.mk_exp(BinOp(LAnd, rexp, checkExp1, ty)) in 
                  let newExp4 = Utils.mk_exp(BinOp(LOr, checkExp2, Utils.mk_exp(UnOp(LNot, rexp, ty)), ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 

		  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "AOR-MinusA-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "AOR-MinusA-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "AOR-MinusA-3" in
		  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "AOR-MinusA-4" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(MinusPI, lexp, rexp, ty) ->
	      if !pmOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp3 = Utils.mk_exp(BinOp(PlusPI, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 

		  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "PM-MPI-1" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "PM-MPI-3" in

		  let ff = ref false in
                 (* if Utils.isPointerRemoveSafe wholeexp e ff then begin 
                     let newExp2 = Utils.mk_exp(rexp.enode) in
                     let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
                     let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "PM-PPI-12" in
		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];
                  end
                  else*) labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt3];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts


	  | BinOp(Mod, lexp, rexp, ty) ->
	      if !aorOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Mult, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(Div, lexp, rexp, ty)) in

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 

		  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "AOR-Mod-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "AOR-Mod-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "AOR-Mod-3" in
		  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "AOR-Mod-4" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts




	  | BinOp(Lt, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
		  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Ne, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp5 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
		  (*let newExp6 = Utils.mk_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let newExp7 = Utils.mk_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let newExp8 = Utils.mk_exp(BinOp(Eq, lexp, rexp, ty)) in
                  let newExp9 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in*)

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  let labelExp5 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp5), wholeexp, ty)) in 

                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "ROR-LT-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "ROR-LT-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "ROR-LT-3" in
                  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "ROR-LT-4" in
		  let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp5) labelExp5 loc "ROR-LT-5" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4; labelStmt5];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(Gt, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
                  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Ne, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let newExp5 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
		  (*let newExp6 = Utils.mk_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp7 = Utils.mk_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp8 = Utils.mk_exp(BinOp(Eq, lexp, rexp, ty)) in
                  let newExp9 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in*)

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  let labelExp5 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp5), wholeexp, ty)) in

                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "ROR-GT-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "ROR-GT-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "ROR-GT-3" in
                  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "ROR-GT-4" in
		  let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp5) labelExp5 loc "ROR-GT-5" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4; labelStmt5];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(Le, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
                  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Eq, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp5 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in
		  (*let newExp6 = Utils.mk_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let newExp7 = Utils.mk_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let newExp8 = Utils.mk_exp(BinOp(Ne, lexp, rexp, ty)) in
                  let newExp9 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in*)

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  let labelExp5 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp5), wholeexp, ty)) in

                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "ROR-LE-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "ROR-LE-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "ROR-LE-3" in
                  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "ROR-LE-4" in
		  let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp5) labelExp5 loc "ROR-LE-5" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4; labelStmt5];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(Ge, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
                  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Eq, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let newExp5 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in
		  (*let newExp6 = Utils.mk_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp7 = Utils.mk_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp8 = Utils.mk_exp(BinOp(Ne, lexp, rexp, ty)) in
                  let newExp9 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in*)

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  let labelExp5 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp5), wholeexp, ty)) in

                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "ROR-GE-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "ROR-GE-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "ROR-GE-3" in
                  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "ROR-GE-4" in
		  let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp5) labelExp5 loc "ROR-GE-5" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4; labelStmt5];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(Eq, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
                  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp5 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
		  (*let newExp6 = Utils.mk_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp7 = Utils.mk_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let newExp8 = Utils.mk_exp(BinOp(Ne, lexp, rexp, ty)) in
                  let newExp9 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in*)

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  let labelExp5 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp5), wholeexp, ty)) in

                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "ROR-Eq-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "ROR-Eq-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "ROR-Eq-3" in
                  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "ROR-Eq-4" in
		  let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp5) labelExp5 loc "ROR-Eq-5" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4; labelStmt5];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts

	  | BinOp(Ne, lexp, rexp, ty) ->
	      if !rorOption = true then
		begin
                  let newExp1 = Utils.mk_exp(lexp.enode) in
                  let newExp2 = Utils.mk_exp(rexp.enode) in
		  let newExp3 = Utils.mk_exp(BinOp(Gt, lexp, rexp, ty)) in
		  let newExp4 = Utils.mk_exp(BinOp(Lt, lexp, rexp, ty)) in
		  let newExp5 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in
		  (*let newExp6 = Utils.mk_exp(BinOp(Ge, lexp, rexp, ty)) in
		  let newExp7 = Utils.mk_exp(BinOp(Le, lexp, rexp, ty)) in
		  let newExp8 = Utils.mk_exp(BinOp(Eq, lexp, rexp, ty)) in
                  let newExp9 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in*)

		  let labelExp1 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp1), wholeexp, ty)) in 
		  let labelExp2 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp2), wholeexp, ty)) in 
		  let labelExp3 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp3), wholeexp, ty)) in 
		  let labelExp4 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp4), wholeexp, ty)) in 
		  let labelExp5 = Utils.mk_exp(BinOp(Ne, (makemutexp wholeexp e newExp5), wholeexp, ty)) in

                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp1) labelExp1 loc "ROR-NE-1" in
		  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp2) labelExp2 loc "ROR-NE-2" in
		  let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp3) labelExp3 loc "ROR-NE-3" in
                  let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp4) labelExp4 loc "ROR-NE-4" in
		  let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e newExp5) labelExp5 loc "ROR-NE-5" in

		    labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4; labelStmt5];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts


	  | BinOp(_op, lexp, rexp, _ty) ->
	      labelsStmts := List.append !labelsStmts (traitExp lexp loc wholeexp);
	      labelsStmts := List.append !labelsStmts (traitExp rexp loc wholeexp);
	      !labelsStmts
	
	  | UnOp(Neg, exp, ty) ->
	      if !aorOption = true then
		begin
		  let labelExp = Utils.mk_exp(BinOp(Ne, wholeexp,  (makemutexp wholeexp e exp), ty)) in  
		  let labelStmt = makeLabel stmt wholeexp (makemutexp wholeexp e exp) labelExp loc "AOR" in
		    labelsStmts := List.append !labelsStmts [labelStmt];
		end;
	      labelsStmts := List.append !labelsStmts (traitExp exp loc wholeexp);
	      !labelsStmts
	

         | UnOp(LNot, exp, ty) ->
              if !uoiOption = true then
	      begin 
		  let labelExp = Utils.mk_exp(BinOp(Ne, wholeexp,  (makemutexp wholeexp e exp), ty)) in  
		  let labelStmt = makeLabel stmt wholeexp (makemutexp wholeexp e exp) labelExp loc "UOI" in
	      labelsStmts := List.append !labelsStmts [labelStmt]
              end;
	      !labelsStmts

	  | UnOp(_op, exp, _ty)->
	      traitExp exp loc wholeexp

	  | Lval(_l) ->
            let typ = Cil.typeOf e in
            if ( Cil.isArithmeticType typ ) then 
            begin 
	    if !absOption = true then
	      begin (* Todo call to function ABS and -ABS *)
		let zeroExp = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
                (*let newExp1 = Utils.mk_exp(UnOp(Neg, e, intType)) in*)
		(*let newExp2 = Utils.mk_exp (Utils.mk_call "abs" [ e ]) in 
		let newExp3 = Utils.mk_exp (UnOp(Neg, (Utils.mk_call "abs" [ e ]), intType)) in*)
		
                if Utils.isdivisionSafe wholeexp e then begin
		let labelExp1 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e zeroExp), intType)) in
		(*let labelExp2 = Utils.mk_exp(BinOp(Gt, e, zeroExp, intType)) in
		let labelExp3 = Utils.mk_exp(BinOp(Eq, e, zeroExp, intType)) in*)

		let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e zeroExp) labelExp1 loc "ABS1" in
 		(*let labelStmt2 = makeLabel stmt wholeexp  (makemutexp wholeexp e newExp2) labelExp2 loc "ABS2" in
		let labelStmt3 = makeLabel stmt wholeexp  (makemutexp wholeexp e newExp3) labelExp3 loc "ABS3" in*)

		  (*labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3];*)
                labelsStmts := List.append !labelsStmts [labelStmt1];
                end
	      end;
	    if !vrrLOption = true then
	      begin
                let fa vars = 
                  let (ll1, ll2) = vars.vdecl in
                  let lnum1 = ll1.Lexing.pos_lnum in
                   let (lle1, lle2) = e.eloc in
                  let lnum2 = lle1.Lexing.pos_lnum in
                  if (not (Cil.appears_in_expr vars e)) && ( lnum1 > 0 ) && ( lnum1 < lnum2 )  && ( Utils.isSameType typ vars.vtype)  then begin
(*Options.feedback "..... %s %d %d" vars.vname lnum1 lnum2;*)
		    let mutexp = Cil.evar vars in
		    let labelExp = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp), intType)) in
                    let labelStmt = makeLabel stmt wholeexp  (makemutexp wholeexp e mutexp) labelExp loc "vrrL" in
		    labelsStmts := List.append !labelsStmts [labelStmt]
	          end in
                List.iter fa !localVars;
            end;
	    if !vrrGOption = true then
	      begin
                let mm vi ii =  
                  if not (Cil.appears_in_expr vi e) then begin
		    let mutexp = Cil.evar vi in
		    let labelExp = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp), intType)) in
                    let labelStmt = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp) labelExp loc "vrrG" in
		    labelsStmts := List.append !labelsStmts [labelStmt]
	          end in
                  Globals.Vars.iter mm;
              end;
              if !uoiOption = true then
	      begin 

                let mutexp1 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(1), IInt, None))), intType)) in
                let mutexp2 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(-1), IInt, None))), intType)) in
                if Utils.isdivisionSafe wholeexp e then begin
		  let labelExp1 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp1), intType)) in
                  let labelExp2 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp2), intType)) in
                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp1) labelExp1 loc "UOI1" in
                  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp2) labelExp2 loc "UOI2" in
	          labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2]
                end
                else begin
                  let labelExp1 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp1), intType)) in
                  let checkExp1 = Utils.mk_exp(BinOp(LAnd, mutexp1, labelExp1, intType)) in 
                  let mutantLabel1 = Utils.mk_exp(BinOp(LOr, checkExp1, Utils.mk_exp(UnOp(LNot, mutexp1, intType)), intType)) in
                  
                  let labelExp2 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp2), intType)) in
                  let checkExp2 = Utils.mk_exp(BinOp(LAnd, mutexp2, labelExp2, intType)) in 
                  let mutantLabel2 = Utils.mk_exp(BinOp(LOr, checkExp2, Utils.mk_exp(UnOp(LNot, mutexp2, intType)), intType)) in

                  let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp1) mutantLabel1 loc "UOI1" in
                  let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp2) mutantLabel2 loc "UOI2" in
	          labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2]
                end
              end;
          end;
	      !labelsStmts

          | Const(c) ->
 	    if !crcOption = true then 
	    begin
            let typ = Cil.typeOf e in
            if ( Cil.isArithmeticType typ ) then 
            begin 
(*todo replace one constant value with another one*)
            match c with 
	       | CInt64(_tt, _IInt, _options) ->
if Utils.isdivisionSafe wholeexp e then begin
                 let mutexp1 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
                 let mutexp2 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in
                 let mutexp3 = Utils.mk_exp (Const(CInt64(Integer.of_int(-1),IInt,None))) in
                 let mutexp4 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(1), IInt, None))), intType)) in
                 let mutexp5 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(-1), IInt, None))), intType)) in
                 let mutexp6 = Utils.mk_exp(UnOp(Neg, e, intType)) in

                 let labelExp1 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp1), intType)) in
                 let labelExp2 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp2), intType)) in
                 let labelExp3 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp3), intType)) in
                 let labelExp4 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp4), intType)) in
                 let labelExp5 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp5), intType)) in
                 let labelExp6 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp6), intType)) in

                 let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp1) labelExp1 loc "CRC1" in
                 let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp2) labelExp2 loc "CRC2" in
                 let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp3) labelExp3 loc "CRC3" in
                 let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp4) labelExp4 loc "CRC4" in
                 let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp5) labelExp5 loc "CRC5" in
                 let labelStmt6 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp6) labelExp6 loc "CRC6" in
        	         labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4; labelStmt5; labelStmt6]
                 end else begin 
                 let mutexp2 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in
                 let mutexp3 = Utils.mk_exp (Const(CInt64(Integer.of_int(-1),IInt,None))) in
                 let mutexp4 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(1), IInt, None))), intType)) in
                 let mutexp5 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(-1), IInt, None))), intType)) in
                 let mutexp6 = Utils.mk_exp(UnOp(Neg, e, intType)) in

                 
                 let labelExp2 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp2), intType)) in
                 let labelExp3 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp3), intType)) in
                 let labelExp4 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp4), intType)) in
                 let labelExp5 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp5), intType)) in
                 let labelExp6 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp6), intType)) in

                 
                 let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp2) labelExp2 loc "CRC2" in
                 let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp3) labelExp3 loc "CRC3" in
                 let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp4) labelExp4 loc "CRC4" in
                 let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp5) labelExp5 loc "CRC5" in
                 let labelStmt6 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp6) labelExp6 loc "CRC6" in
labelsStmts := List.append !labelsStmts [labelStmt2; labelStmt3; labelStmt4; labelStmt5; labelStmt6]
end

               | CEnum(_enumitem) -> 
	   (* if !vrrLOption = true then
	      begin
                let fa vars = 
                  let (ll1, ll2) = vars.vdecl in
                  let lnum1 = ll1.Lexing.pos_lnum in
                   let (lle1, lle2) = e.eloc in
                  let lnum2 = lle1.Lexing.pos_lnum in
                  if (not (Cil.appears_in_expr vars e)) && ( lnum1 > 0 ) && ( lnum1 < lnum2 )  && ( Utils.isSameType typ vars.vtype)  then begin
(*Options.feedback "..... %s %d %d" vars.vname lnum1 lnum2;*)
		    let mutexp = Cil.evar vars in
		    let labelExp = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp), intType)) in
                    let labelStmt = makeLabel stmt wholeexp  (makemutexp wholeexp e mutexp) labelExp loc "vrrLa" in
		    labelsStmts := List.append !labelsStmts [labelStmt]
	          end in
                List.iter fa !localVars;
             end
*)
                let mutexp1 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
                 let mutexp2 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in
                 let mutexp3 = Utils.mk_exp (Const(CInt64(Integer.of_int(-1),IInt,None))) in
                 let mutexp4 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(1), IInt, None))), intType)) in
                 let mutexp5 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(-1), IInt, None))), intType)) in
                 let mutexp6 = Utils.mk_exp(UnOp(Neg, e, intType)) in

                 let labelExp1 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp1), intType)) in
                 let labelExp2 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp2), intType)) in
                 let labelExp3 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp3), intType)) in
                 let labelExp4 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp4), intType)) in
                 let labelExp5 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp5), intType)) in
                 let labelExp6 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp6), intType)) in

                 let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp1) labelExp1 loc "CRC1" in
                 let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp2) labelExp2 loc "CRC2" in
                 let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp3) labelExp3 loc "CRC3" in
                 let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp4) labelExp4 loc "CRC4" in
                 let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp5) labelExp5 loc "CRC5" in
                 let labelStmt6 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp6) labelExp6 loc "CRC6" in
        	         labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4; labelStmt5; labelStmt6]
               


 	       | CReal(_tt, _Fkind, _options) -> 
if Utils.isdivisionSafe wholeexp e then begin
                 let mutexp1 = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
                 let mutexp2 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in
                 let mutexp3 = Utils.mk_exp (Const(CInt64(Integer.of_int(-1),IInt,None))) in
                 let mutexp4 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(1), IInt, None))), intType)) in
                 let mutexp5 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(-1), IInt, None))), intType)) in
                 let mutexp6 = Utils.mk_exp(UnOp(Neg, e, intType)) in

                 let labelExp1 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp1), intType)) in
                 let labelExp2 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp2), intType)) in
                 let labelExp3 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp3), intType)) in
                 let labelExp4 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp4), intType)) in
                 let labelExp5 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp5), intType)) in
                 let labelExp6 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp6), intType)) in

                 let labelStmt1 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp1) labelExp1 loc "CRC1" in
                 let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp2) labelExp2 loc "CRC2" in
                 let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp3) labelExp3 loc "CRC3" in
                 let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp4) labelExp4 loc "CRC4" in
                 let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp5) labelExp5 loc "CRC5" in
                 let labelStmt6 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp6) labelExp6 loc "CRC6" in
	         labelsStmts := List.append !labelsStmts [labelStmt1; labelStmt2; labelStmt3; labelStmt4; labelStmt5; labelStmt6]
end else begin 
                
                 let mutexp2 = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None))) in
                 let mutexp3 = Utils.mk_exp (Const(CInt64(Integer.of_int(-1),IInt,None))) in
                 let mutexp4 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(1), IInt, None))), intType)) in
                 let mutexp5 = Utils.mk_exp(BinOp(PlusPI, e, Utils.mk_exp (Const(CInt64(Integer.of_int(-1), IInt, None))), intType)) in
                 let mutexp6 = Utils.mk_exp(UnOp(Neg, e, intType)) in

                 let labelExp2 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp2), intType)) in
                 let labelExp3 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp3), intType)) in
                 let labelExp4 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp4), intType)) in
                 let labelExp5 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp5), intType)) in
                 let labelExp6 = Utils.mk_exp(BinOp(Ne, wholeexp, (makemutexp wholeexp e mutexp6), intType)) in

                 let labelStmt2 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp2) labelExp2 loc "CRC2" in
                 let labelStmt3 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp3) labelExp3 loc "CRC3" in
                 let labelStmt4 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp4) labelExp4 loc "CRC4" in
                 let labelStmt5 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp5) labelExp5 loc "CRC5" in
                 let labelStmt6 = makeLabel stmt wholeexp (makemutexp wholeexp e mutexp6) labelExp6 loc "CRC6" in
	         labelsStmts := List.append !labelsStmts [labelStmt2; labelStmt3; labelStmt4; labelStmt5; labelStmt6]
end

 	       | _ -> ()
           end;
           end;
            !labelsStmts

          | CastE(typ, exp) -> traitExp exp loc wholeexp

	  | _ -> []
	end
    in
    let traitStmt isBooleanCond ?(otherlabels=[]) s e loc = 
      let labelsList1 = traitExp e loc e in
      let labelsList = List.append otherlabels labelsList1 in
      if isBooleanCond && (List.length labelsList)>1 then begin (List.iter (fun l-> Format.fprintf !Utils.formatter "%d, " (get_label_id l)) labelsList); Format.fprintf !Utils.formatter " @." end;
        match labelsList with 
	  | [] -> s
	  | _ ->
	      let finalList = List.append  labelsList [s] in
	      let b2 = mkBlock finalList in
	      let i = mkStmt (Block(b2)) in	  
              mylabels := Labels.add s.sid i !mylabels;s(*i*)
    in
      let sdlmutation s e loc = 
        if !sdlOption = true then begin
          let dummy = Cil.invalidStmt in 
          let labelExp = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None)))  in
          let labelStmt = makeLabel ~origstmt:s ~mutstmt:dummy s e e labelExp loc "SSDL1" in
          traitStmt false ~otherlabels:[labelStmt] s e loc
         (* let b2 = mkBlock [labelStmt] in
	  let i = mkStmt (Block(b2)) in	  
          mylabels := Labels.add s.sid i !mylabels;s(*i*)
*)
        end 
	else
          traitStmt false s e loc
in
let sdlmutation2 s loc = 
        if !sdlOption = true then begin
          let dummy = Cil.invalidStmt in 
          let labelExp = Utils.mk_exp (Const(CInt64(Integer.of_int(1),IInt,None)))  in
          let labelStmt = makeLabel ~origstmt:s ~mutstmt:dummy s labelExp labelExp labelExp loc "SSDL2" in
          let finalList = List.append  [labelStmt] [s] in
          let b2 = mkBlock finalList in
	  let i = mkStmt (Block(b2)) in	  
          mylabels := Labels.add s.sid i !mylabels;s(*i*)
        end 
        else s
in
    let genLabels s =
      match s.skind with
	| Instr(Set(_l, e, loc)) ->
	    (*traitStmt s e loc*)
            sdlmutation s e loc

	| Instr(Call(_l, e, args, loc)) -> sdlmutation s e loc
		
	| If(e, _l, _ll, loc) ->
	    traitStmt true s e loc;(*let zeroExp = Utils.mk_exp (Const(CInt64(Integer.of_int(0),IInt,None))) in
		s.skind <- If(zeroExp, _l, _ll, loc);s*)

	(*| Loop(ann_list, block, loc, stmtopt1, stmtopt2) -> *) 
	     
        | Continue(loc) -> sdlmutation2 s loc

        | Break(loc) -> sdlmutation2 s loc

	| Return(e, loc) ->
	    begin match e with 
	      | Some exp -> s
(* todo consider return mutation, now is ignored
		  traitStmt s exp loc *)
	      | _ -> s 
	    end
	    
	      
	| Switch(e, _block, _stmLists, loc) ->
	    traitStmt true s e loc ; 
	      
      | _ -> s
    in  
      DoChildrenPost genLabels
      (*ChangeDoChildrenPost (stmt, genLabels)*)
end

  let compute mk_label ast =
    Visitor.visitFramacFileSameGlobals
      (new strongmutationVisitor mk_label :> Visitor.frama_c_inplace)
      ast
end)


(*****************************************)

module WM = Annotators.RegisterWithExtraTags (struct

  let name = "WM"
  let help = "Weak Mutation"

  class mutationVisitor mk_label = object(_self)
    inherit Visitor.frama_c_inplace

    method! vfunc f =
      if Annotators.shouldInstrument f.svar then DoChildren else SkipChildren

  method! vstmt_aux stmt =      	  
    let makeLabel labelExp loc category = mk_label ~extra:[category] labelExp loc in
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

