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

let pos atom = Cil.copy_exp atom
let neg atom = Utils.mk_exp (UnOp (LNot, Cil.copy_exp atom, Cil.intType))

(**
  Generates labels for n-CC coverage from a boolean expression.
  And puts them all in a single block statement.
*)
let gen_labels_ncc mk_label n (bexpr : exp) : stmt =
  let loc = bexpr.eloc in
  let atoms = atomic_conditions bexpr in
  let natoms = List.length atoms in
  Options.debug3 "%d atoms in @[%a@]" natoms Printer.pp_exp bexpr;

  (* Compute subsets of m atoms *)
  let n = if n <= 0 then natoms else min n natoms in
  let subsets = combine n atoms in
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
    (* Compute signed subsets *)
    let signed_subsets = sign_combine pos neg subset in
    (* Create labels for each signed subset (taken in rev. order)
      and put them in rev. in front of acc (N.B. [rev rev l = l]) *)
    List.fold_left for_signed_subset acc signed_subsets
  in
  let stmts = List.rev (List.fold_left for_subset [] subsets) in
  mk_block_stmt stmts;;

let gen_labels_dc mk_label bexpr =
  let loc = bexpr.eloc in
  let labels = [mk_label (pos bexpr) loc; mk_label (neg bexpr) loc] in
  mk_block_stmt labels;;

(**
 * Frama-C in-place visitor that injects labels at each conditition/boolean
 * expression using some injection function
 *)
class visitor gen_labels all_boolean = object(self)
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
      let labels = mk_block_stmt (List.rev_map gen_labels bexprs) in
      bexprs <- [];
      mk_block_stmt [labels; stmt]

  method! vstmt_aux stmt =
    match stmt.skind with
    | If (e, thenb, elseb, loc) ->
      let labels_stmt = gen_labels e in
      (* handle visits manually to skip visit of e *)
      let thenb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) thenb in
      let elseb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) elseb in
      stmt.skind <- If (e, thenb, elseb, loc);
      Cil.ChangeTo (mk_block_stmt [labels_stmt; stmt])
    | _ ->
      if all_boolean then
        Cil.DoChildrenPost (fun stmt -> self#vstmt_post stmt)
      else
        Cil.DoChildren

  method! vinst instr =
    if is_label instr then Cil.SkipChildren
    else Cil.DoChildren

  method! vexpr expr =
    if all_boolean && is_boolean expr then begin
      bexprs <- expr :: bexprs;
      Cil.SkipChildren
    end else
      Cil.DoChildren
end

(** Generic condition/boolean expression annotator *)
let apply gen_labels all_boolean file =
    Visitor.visitFramacFileSameGlobals (new visitor gen_labels all_boolean :> Visitor.frama_c_visitor) file

(** n-CC condition/boolean expression annotator *)
let apply_ncc mk_label n all_boolean file =
  Options.debug2 "n-Condition Coverage config: n=%d, all booleans=%B" n all_boolean;
  apply (gen_labels_ncc mk_label n) all_boolean file

(**
 * Condition coverage annotator, special case of n-CC for n=1
 *)
module CC = Annotators.Register (struct
  let name = "CC"
  let help = "Condition Coverage"

  let apply mk_label file =
    apply_ncc mk_label 1 (Options.AllBoolExps.get ()) file
end)

(**
 * n-wise condition coverage annotator
 *)
module NCC = Annotators.Register (struct
  let name = "NCC"
  let help = "n-wise Condition Coverage"

  let apply mk_label file =
    apply_ncc mk_label (Options.N.get ()) (Options.AllBoolExps.get ()) file
end)

(**
 * Multiple condition coverage annotator, special case of n-CC for n=infinite
 * (coded zero)
 *)
module MCC = Annotators.Register (struct
  let name = "MCC"
  let help = "Multiple Condition Coverage"

  let apply mk_label file =
    apply_ncc mk_label 0 (Options.AllBoolExps.get ()) file
end)

module DC = Annotators.Register (struct
  let name = "DC"
  let help = "Decision Coverage"
  let apply mk_label file =
    apply (gen_labels_dc mk_label) (Options.AllBoolExps.get ()) file
end)
