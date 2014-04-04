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

open Cil_types
open Utils

module type CONFIG = sig
  val n : int
  val all_boolean : bool
  val mk_label : exp -> location -> stmt
end

module GenericConditionCoverage (C : CONFIG) = struct
  open C

  let pos atom = Cil.copy_exp atom
  let neg atom = Utils.mk_exp (UnOp (LNot, Cil.copy_exp atom, Cil.intType))

  (**
    Generates labels from a boolean expression.
    Adds label statements in front of the provided accumalator.
  *)
  let gen_labels (acc : stmt list) (bexpr : exp) : stmt list =
    let loc = bexpr.eloc in
    let atoms = atomic_conditions bexpr in
    let natoms = List.length atoms in
    Options.debug3 "%d atoms in @[%a@]" natoms Cil_printer.pp_exp bexpr;

    (* Compute subsets of m atoms *)
    let n = if n <= 0 then natoms else min n natoms in
    let subsets = rev_combine n atoms in
    Options.debug2 "%d subsets of %d atoms" (List.length subsets) n;

    (* For each signed subset of atoms, *)
    let for_signed_subset (acc : stmt list) (signed_subset : exp list) : stmt list =
      (* Get conjunction as an expression*)
      let exp = andify signed_subset in
      (* Create a label and put it in front of acc *)
      mk_label exp loc :: acc
    in

    (* For each subset of atoms, *)
    let for_subset (acc : stmt list) (subset : exp list) : stmt list =
      (* Compute signed subsets in reverse order *)
      let signed_subsets = rev_sign_combine pos neg subset in
      (* Create labels for each signed subset (taken in rev. order)
        and put them in rev. in front of acc (N.B. [rev rev l = l]) *)
      List.fold_left for_signed_subset acc signed_subsets
    in
    List.fold_left for_subset acc subsets;;

  class visitor = object(self)
    inherit Visitor.frama_c_inplace

    val mutable bexprs = []

    method! vfunc dec =
      if Annotators.shouldInstrument dec.svar then
        Cil.DoChildren
      else
        Cil.SkipChildren

    method vstmt_post stmt =
      match bexprs with
      | [] -> stmt
      | _ ->
        let stmts = List.fold_left gen_labels [stmt] bexprs in
        bexprs <- [];
        Cil.mkStmt (Block (Cil.mkBlock stmts))

    method! vstmt_aux stmt =
      match stmt.skind with
      | If (e, thenb, elseb, loc) ->
        let stmts = gen_labels [stmt] e in
        (* handle visits manually to skip visit of e *)
        let thenb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) thenb in
        let elseb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) elseb in
        stmt.skind <- If (e, thenb, elseb, loc);
        Cil.ChangeTo (Cil.mkStmt (Block (Cil.mkBlock stmts)))
      | _ ->
        if all_boolean then
          Cil.DoChildrenPost (fun stmt -> self#vstmt_post stmt)
        else
          Cil.DoChildren

    method! vexpr expr =
      if all_boolean && is_boolean expr then begin
        bexprs <- expr :: bexprs;
        Cil.SkipChildren
      end else
        Cil.DoChildren
  end

  let apply file =
    Visitor.visitFramacFileSameGlobals (new visitor :> Visitor.frama_c_visitor) file
end

let apply n all_boolean mk_label file =
  Options.debug2 "n-Condition Coverage config: n=%d, all booleans=%B" n all_boolean;
  let module G = GenericConditionCoverage (struct
    let n = n
    let all_boolean = all_boolean
    let mk_label = mk_label
  end) in
  G.apply file

module CC = Annotators.Register (struct
  let name = "CC"
  let help = "Condition Coverage"

  let compute mk_label file =
    apply 1 (Options.AllBoolExps.get ()) mk_label file
end)

module NCC = Annotators.Register (struct
  let name = "NCC"
  let help = "n-Condition Coverage"

  let compute mk_label file =
    apply (Options.N.get ()) (Options.AllBoolExps.get ()) mk_label file
end)

module MCC = Annotators.Register (struct
  let name = "MCC"
  let help = "Multiple Condition Coverage"

  let compute mk_label file =
    apply 0 (Options.AllBoolExps.get ()) mk_label file
end)
