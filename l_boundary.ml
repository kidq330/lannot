(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2020                                               *)
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
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Ast_const

let unk_loc = Cil_datatype.Location.unknown

(** Input Output Bound Visitor **)
class visitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  (* For each exp, limits equals [min;max] (signed) or [max] (unsigned)
     We create a label for each bound, plus a label for zero (which is
     min for unsigned types) *)
  method private mk_bound (limits:Integer.t list) (exp:exp) (ik:ikind) (loc:location) : stmt list =
    let zero = mk_label (Exp.binop Eq exp (Exp.zero ())) [] loc in
    let exp_limits =
      List.map (fun i -> Exp.kinteger64 ik i) limits
    in
    let lbl_limits =
      List.map (fun exp' ->
          mk_label (Exp.binop Eq exp exp') [] loc
        ) exp_limits
    in
    zero :: lbl_limits

  (* Create bound labels for each "int type" argument *)
  method private mk_bounds (loc:location) (exp:exp) : stmt list =
    match Cil.typeOf exp with
    | TInt (kind,_) ->
      let size = Cil.bitsSizeOfInt kind in
      let limits =
        if Cil.isSigned kind then
          [Cil.min_signed_number size;
           Cil.max_signed_number size]
        else
          [Cil.max_unsigned_number size]
      in
      self#mk_bound limits exp kind loc
    | _ -> []

  (* visit each function and annotate formals and return statement*)
  method! vfunc _ =
    let kf = Extlib.the self#current_kf in
    let args = Kernel_function.get_formals kf in
    let exp_args = List.map (fun arg -> Exp.lval (Lval.var arg)) args in
    let loc = Kernel_function.get_location kf in
    let bounds_labels = List.concat_map (self#mk_bounds loc) exp_args in
    Cil.DoChildrenPost( fun fdec ->
        fdec.sbody.bstmts <- bounds_labels @ fdec.sbody.bstmts;
        fdec
    )

  (* Create bound labels for return statement  *)
  method! vstmt_aux stmt =
    begin match stmt.skind with
      | Return (Some exp, loc) ->
        let bounds_labels = self#mk_bounds loc exp in
        stmt.skind <- Block (Block.mk (bounds_labels @ [Stmt.mk stmt.skind]));
        Cil.SkipChildren
      | _ -> Cil.DoChildren
    end

end

(**
   Input Output Bounds annotator
*)
module InOutBound = Annotators.Register (struct
    let name = "IOB"
    let help = "Input Output Bounds Coverage"
    let apply mk_label file =
      Visitor.visitFramacFileSameGlobals (new visitor mk_label :> Visitor.frama_c_visitor) file
  end)
