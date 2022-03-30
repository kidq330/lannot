(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's Lannotate plug-in.                 *)
(*                                                                        *)
(*  Copyright (C) 2012-2022                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file LICENSE)                       *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Ast_const

(* For each exp, limits equals [min;max] (signed) or [max] (unsigned)
   We create a label for each bound, plus a label for zero (which is
   min for unsigned types) *)
let mk_bounds mk_label (loc:location) (exp:exp) : stmt list =
  match Cil.typeOf exp with
  | TInt (kind, _) ->
    Utils.get_bounds kind |>
    List.map (fun (op,exp') ->
        mk_label (Exp_builder.binop op exp exp') [] loc
      )
  | _ -> []

(** Input Output Bound Visitor **)
class io_visitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  (* visit each function and annotate formals and return statement*)
  method! vfunc dec =
    if not @@ Annotators.shouldInstrumentFun dec.svar then
      Cil.SkipChildren
    else
      let kf = Option.get self#current_kf in
      let args = Kernel_function.get_formals kf in
      let exp_args = List.map (fun arg -> Exp_builder.lval (Cil.var arg)) args in
      let loc = Kernel_function.get_location kf in
      let bounds_labels =
        Transitioning.List.concat_map (mk_bounds mk_label loc) exp_args
      in
      Cil.DoChildrenPost( fun fdec ->
          fdec.sbody.bstmts <- bounds_labels @ fdec.sbody.bstmts;
          fdec
        )

  (* Create bound labels for return statement  *)
  method! vstmt_aux stmt =
    begin match stmt.skind with
      | Return (Some exp, loc) ->
        let bounds_labels = mk_bounds mk_label loc exp in
        stmt.skind <- Block (Cil.mkBlock (bounds_labels @ [Stmt_builder.mk stmt.skind]));
        Cil.SkipChildren
      | _ -> Cil.DoChildren
    end
end


(** Condition Bound Visitor **)
class atom_visitor mk_label labels = object(self)
  inherit Visitor.frama_c_inplace

  method! vexpr exp =
    match exp.enode with
    | Lval _ ->
      labels := List.rev (mk_bounds mk_label exp.eloc exp) @ !labels;
      Cil.SkipChildren
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), e1, e2, _) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) e1);
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) e2);
      let new_exp = Exp_builder.binop Eq e1 e2 in
      labels := (mk_label new_exp [] exp.eloc) :: !labels;
      Cil.SkipChildren
    | BinOp ((LAnd|LOr), e1, e2, _) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) e1);
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) e2);
      let new_exp1 = Exp_builder.lnot e1 in
      let new_exp2 = Exp_builder.lnot e2 in
      labels := (mk_label new_exp2 [] exp.eloc) :: (mk_label new_exp1 [] exp.eloc) :: !labels;
      Cil.SkipChildren
    | UnOp (LNot, e1, _) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) e1);
      let new_exp = Exp_builder.binop Eq e1 (Exp_builder.zero()) in
      labels := (mk_label new_exp [] exp.eloc) :: !labels;
      Cil.SkipChildren
    | _ -> Cil.DoChildren
end

(** Condition Bound Visitor **)
class c_visitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  method! vfunc dec =
    if Annotators.shouldInstrumentFun dec.svar then
      Cil.DoChildren
    else
      Cil.SkipChildren

  (* Create bound labels for return statement  *)
  method! vstmt_aux stmt =
    begin match stmt.skind with
      | If (e, thenb, elseb, loc) ->
        let atoms_labels = ref [] in
        ignore(Cil.visitCilExpr (new atom_visitor mk_label atoms_labels :> Cil.cilVisitor) e);
        (* handle visits manually to skip visit of e *)
        let thenb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) thenb in
        let elseb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) elseb in
        let old_stmt = Stmt_builder.mk (If (e, thenb, elseb, loc)) in
        stmt.skind <- Block (Cil.mkBlock (List.rev !atoms_labels @ [old_stmt]));
        Cil.SkipChildren
      | _ -> Cil.DoChildren
    end
end

(**
   Input Output Bound annotator
*)
module InOutBound = Annotators.Register (struct
    let name = "IOB"
    let help = "Input Output Bound Coverage"
    let apply mk_label file =
      Visitor.visitFramacFileSameGlobals (new io_visitor mk_label :> Visitor.frama_c_visitor) file
  end)


(**
   Condition Bound annotator
*)
module CondBound = Annotators.Register (struct
    let name = "CB"
    let help = "Condition Bound Coverage"
    let apply mk_label file =
      Visitor.visitFramacFileSameGlobals (new c_visitor mk_label :> Visitor.frama_c_visitor) file
  end)

(**
   Bound annotator
*)
module Bound = Annotators.Register (struct
    let name = "BC"
    let help = "Bound Coverage (IOB + CB)"
    let apply mk_label file =
      Visitor.visitFramacFileSameGlobals (new io_visitor mk_label :> Visitor.frama_c_visitor) file;
      Visitor.visitFramacFileSameGlobals (new c_visitor mk_label :> Visitor.frama_c_visitor) file
  end)
