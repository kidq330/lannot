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
          let mk_op_labels lop lexp rexp ty wm opt =
              if opt = true then begin
                let lwm = List.map (fun op ->
                    let newExp = Ast_const.Exp.mk(BinOp(op, lexp, rexp, ty)) in
                    let labelExp = Ast_const.Exp.mk(BinOp(Ne, newExp, e, ty)) in
                    makeLabel labelExp loc wm
                  ) lop in
                labelsStmts := List.append !labelsStmts lwm
              end;
              labelsStmts := List.append !labelsStmts (traitExp lexp loc);
              labelsStmts := List.append !labelsStmts (traitExp rexp loc);
              !labelsStmts
          in
          begin match e.enode with
            | BinOp(LAnd, lexp, rexp, ty) ->
              mk_op_labels [LOr] lexp rexp ty "COR" !corOption

            | BinOp(LOr, lexp, rexp, ty) ->
              mk_op_labels [LAnd] lexp rexp ty "COR" !corOption

            | BinOp(Div, lexp, rexp, ty) ->
              mk_op_labels [Mult;PlusA;MinusA] lexp rexp ty "AOR" !aorOption

            | BinOp(Mult, lexp, rexp, ty) ->
              mk_op_labels [Div;PlusA;MinusA] lexp rexp ty "AOR" !aorOption

            | BinOp(PlusA, lexp, rexp, ty) ->
              mk_op_labels [Mult;Div;MinusA] lexp rexp ty "AOR" !aorOption

            | BinOp(MinusA, lexp, rexp, ty) ->
              mk_op_labels [Mult;Div;PlusA] lexp rexp ty "AOR" !aorOption

            | BinOp(Lt, lexp, rexp, ty) ->
              mk_op_labels [Le;Gt;Ge] lexp rexp ty "ROR" !rorOption

            | BinOp(Gt, lexp, rexp, ty) ->
              mk_op_labels [Lt;Le;Ge] lexp rexp ty "ROR" !rorOption

            | BinOp(Le, lexp, rexp, ty) ->
              mk_op_labels [Lt;Gt;Ge] lexp rexp ty "ROR" !rorOption

            | BinOp(Ge, lexp, rexp, ty) ->
              mk_op_labels [Lt;Le;Gt] lexp rexp ty "ROR" !rorOption

            | BinOp(Eq, lexp, rexp, ty) ->
              mk_op_labels [Ne] lexp rexp ty "ROR" !rorOption

            | BinOp(Ne, lexp, rexp, ty) ->
              mk_op_labels [Eq] lexp rexp ty "ROR" !rorOption

            | BinOp(Shiftlt, lexp, rexp, ty) ->
              mk_op_labels [Shiftrt] lexp rexp ty "AOR" !aorOption

            | BinOp(Shiftrt, lexp, rexp, ty) ->
              mk_op_labels [Shiftlt] lexp rexp ty "AOR" !aorOption

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
