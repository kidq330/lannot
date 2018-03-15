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

let aorOption : bool ref = ref false
let rorOption : bool ref = ref false
let corOption : bool ref = ref false
let absOption : bool ref = ref false

module WM = Annotators.RegisterWithExtraTags (struct

    let name = "WM"
    let help = "Weak Mutation"

    class mutationVisitor mk_label = object(_self)
      inherit Visitor.frama_c_inplace

      method! vfunc f =
        if Annotators.shouldInstrument f.svar then DoChildren else SkipChildren

      method! vstmt_aux stmt =
        let makeLabel cond loc category = mk_label ~extra:[category] cond [] loc in
        let rec traitExp e loc =
          let labelsStmts = ref [] in
          begin match e.enode with
            | BinOp(LAnd, lexp, rexp, ty) ->
              if !corOption = true then
                begin
                  let newEnode = BinOp(LOr, lexp, rexp, ty) in
                  let newExp = {e with enode = newEnode} in
                  let labelExp = Ast_const.Exp.mk(BinOp(Ne, newExp, e, ty)) in
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
                  let labelExp = Ast_const.Exp.mk(BinOp(Ne, newExp, e, ty)) in
                  let labelStmt = makeLabel labelExp loc "COR" in
                  labelsStmts := List.append !labelsStmts [labelStmt];
                end;
              labelsStmts := List.append !labelsStmts (traitExp lexp loc);
              labelsStmts := List.append !labelsStmts (traitExp rexp loc);
              !labelsStmts

            | BinOp(Div, lexp, rexp, ty) ->
              if !aorOption = true then
                begin
                  let newExp1 = Ast_const.Exp.mk(BinOp(Mult, lexp, rexp, ty)) in
                  let newExp2 = Ast_const.Exp.mk(BinOp(PlusA, lexp, rexp, ty)) in
                  let newExp3 = Ast_const.Exp.mk(BinOp(MinusA, lexp, rexp, ty)) in
                  let labelExp1 = Ast_const.Exp.mk(BinOp(Ne, newExp1, e, ty)) in
                  let labelExp2 = Ast_const.Exp.mk(BinOp(Ne, newExp2, e, ty)) in
                  let labelExp3 = Ast_const.Exp.mk(BinOp(Ne, newExp3, e, ty)) in
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
                  let newExp1 = Ast_const.Exp.mk(BinOp(Div, lexp, rexp, ty)) in
                  let newExp2 = Ast_const.Exp.mk(BinOp(PlusA, lexp, rexp, ty)) in
                  let newExp3 = Ast_const.Exp.mk(BinOp(MinusA, lexp, rexp, ty)) in
                  let labelExp1 = Ast_const.Exp.mk(BinOp(Ne, newExp1, e, ty)) in
                  let labelExp2 = Ast_const.Exp.mk(BinOp(Ne, newExp2, e, ty)) in
                  let labelExp3 = Ast_const.Exp.mk(BinOp(Ne, newExp3, e, ty)) in
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
                  let newExp1 = Ast_const.Exp.mk(BinOp(Mult, lexp, rexp, ty)) in
                  let newExp2 = Ast_const.Exp.mk(BinOp(Div, lexp, rexp, ty)) in
                  let newExp3 = Ast_const.Exp.mk(BinOp(MinusA, lexp, rexp, ty)) in
                  let labelExp1 = Ast_const.Exp.mk(BinOp(Ne, newExp1, e, ty)) in
                  let labelExp2 = Ast_const.Exp.mk(BinOp(Ne, newExp2, e, ty)) in
                  let labelExp3 = Ast_const.Exp.mk(BinOp(Ne, newExp3, e, ty)) in
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
                  let newExp1 = Ast_const.Exp.mk(BinOp(Mult, lexp, rexp, ty)) in
                  let newExp2 = Ast_const.Exp.mk(BinOp(Div, lexp, rexp, ty)) in
                  let newExp3 = Ast_const.Exp.mk(BinOp(PlusA, lexp, rexp, ty)) in
                  let labelExp1 = Ast_const.Exp.mk(BinOp(Ne, newExp1, e, ty)) in
                  let labelExp2 = Ast_const.Exp.mk(BinOp(Ne, newExp2, e, ty)) in
                  let labelExp3 = Ast_const.Exp.mk(BinOp(Ne, newExp3, e, ty)) in
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
                  let newExp1 = Ast_const.Exp.mk(BinOp(Le, lexp, rexp, ty)) in
                  let newExp2 = Ast_const.Exp.mk(BinOp(Gt, lexp, rexp, ty)) in
                  let newExp3 = Ast_const.Exp.mk(BinOp(Ge, lexp, rexp, ty)) in
                  let labelExp1 = Ast_const.Exp.mk(BinOp(Ne, newExp1, e, ty)) in
                  let labelExp2 = Ast_const.Exp.mk(BinOp(Ne, newExp2, e, ty)) in
                  let labelExp3 = Ast_const.Exp.mk(BinOp(Ne, newExp3, e, ty)) in
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
                  let newExp1 = Ast_const.Exp.mk(BinOp(Lt, lexp, rexp, ty)) in
                  let newExp2 = Ast_const.Exp.mk(BinOp(Le, lexp, rexp, ty)) in
                  let newExp3 = Ast_const.Exp.mk(BinOp(Ge, lexp, rexp, ty)) in
                  let labelExp1 = Ast_const.Exp.mk(BinOp(Ne, newExp1, e, ty)) in
                  let labelExp2 = Ast_const.Exp.mk(BinOp(Ne, newExp2, e, ty)) in
                  let labelExp3 = Ast_const.Exp.mk(BinOp(Ne, newExp3, e, ty)) in
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
                  let newExp1 = Ast_const.Exp.mk(BinOp(Lt, lexp, rexp, ty)) in
                  let newExp2 = Ast_const.Exp.mk(BinOp(Gt, lexp, rexp, ty)) in
                  let newExp3 = Ast_const.Exp.mk(BinOp(Ge, lexp, rexp, ty)) in
                  let labelExp1 = Ast_const.Exp.mk(BinOp(Ne, newExp1, e, ty)) in
                  let labelExp2 = Ast_const.Exp.mk(BinOp(Ne, newExp2, e, ty)) in
                  let labelExp3 = Ast_const.Exp.mk(BinOp(Ne, newExp3, e, ty)) in
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
                  let newExp1 = Ast_const.Exp.mk(BinOp(Lt, lexp, rexp, ty)) in
                  let newExp2 = Ast_const.Exp.mk(BinOp(Le, lexp, rexp, ty)) in
                  let newExp3 = Ast_const.Exp.mk(BinOp(Gt, lexp, rexp, ty)) in
                  let labelExp1 = Ast_const.Exp.mk(BinOp(Ne, newExp1, e, ty)) in
                  let labelExp2 = Ast_const.Exp.mk(BinOp(Ne, newExp2, e, ty)) in
                  let labelExp3 = Ast_const.Exp.mk(BinOp(Ne, newExp3, e, ty)) in
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
                  let newExp = Ast_const.Exp.mk(BinOp(Ne, lexp, rexp, ty)) in
                  let labelExp = Ast_const.Exp.mk(BinOp(Ne, newExp, e, ty)) in
                  let labelStmt = makeLabel labelExp loc "ROR" in
                  labelsStmts := List.append !labelsStmts [labelStmt];
                end;
              labelsStmts := List.append !labelsStmts (traitExp lexp loc);
              labelsStmts := List.append !labelsStmts (traitExp rexp loc);
              !labelsStmts

            | BinOp(Ne, lexp, rexp, ty) ->
              if !rorOption = true then
                begin
                  let newExp = Ast_const.Exp.mk(BinOp(Eq, lexp, rexp, ty)) in
                  let labelExp = Ast_const.Exp.mk(BinOp(Ne, newExp, e, ty)) in
                  let labelStmt = makeLabel labelExp loc "ROR" in
                  labelsStmts := List.append !labelsStmts [labelStmt];
                end;
              labelsStmts := List.append !labelsStmts (traitExp lexp loc);
              labelsStmts := List.append !labelsStmts (traitExp rexp loc);
              !labelsStmts

            | BinOp(Shiftlt, lexp, rexp, ty) ->
              if !aorOption = true then
                begin
                  let newExp = Ast_const.Exp.mk(BinOp(Shiftrt, lexp, rexp, ty)) in
                  let labelExp = Ast_const.Exp.mk(BinOp(Ne, newExp, e, ty)) in
                  let labelStmt = makeLabel labelExp loc "AOR" in
                  labelsStmts := List.append !labelsStmts [labelStmt];
                end;
              labelsStmts := List.append !labelsStmts (traitExp lexp loc);
              labelsStmts := List.append !labelsStmts (traitExp rexp loc);
              !labelsStmts

            | BinOp(Shiftrt, lexp, rexp, ty) ->
              if !aorOption = true then
                begin
                  let newExp = Ast_const.Exp.mk(BinOp(Shiftlt, lexp, rexp, ty)) in
                  let labelExp = Ast_const.Exp.mk(BinOp(Ne, newExp, e, ty)) in
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
                  let labelExp = Ast_const.Exp.mk(BinOp(Ne, exp, e, ty)) in
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
                  let zeroExp = Ast_const.Exp.mk (Const(CInt64(Integer.of_int(0),IInt,None))) in
                  let labelExp = Ast_const.Exp.mk(BinOp(Lt, e, zeroExp, intType)) in
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

    let apply mk_label ast =
      Visitor.visitFramacFileSameGlobals
        (new mutationVisitor mk_label :> Visitor.frama_c_inplace)
        ast
  end)
