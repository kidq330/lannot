(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C/LTest.                                   *)
(*                                                                        *)
(*  Copyright (C) 2013-2014                                               *)
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
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Annotators

include Register (struct

let name = "DC"
let help = "Decision Coverage"

(**
 * Add one label of each branch of a If.
 * Operates IN PLACE (it does returns the parameter though)
 *)
let add_decision_labels mk_label stmt =
  let mk_label_true loc = mk_label (Cil.one (Lexing.dummy_pos, Lexing.dummy_pos)) loc in
  match stmt.skind with
  | If (_, thenBlock, elseBlock, loc) ->
     thenBlock.bstmts <- mk_label_true loc :: thenBlock.bstmts;
     elseBlock.bstmts <- mk_label_true loc :: elseBlock.bstmts;
     stmt
  | _ -> stmt

(** A visitor that calls add_decision_labels AFTER visiting each If *)
let visitor mk_label = object
  inherit Visitor.frama_c_inplace

  method! vfunc dec =
    if shouldInstrument dec.svar then
      Cil.DoChildren
    else
      Cil.SkipChildren

  method! vstmt_aux stmt =
    match stmt.skind with
    | If _ ->
      Cil.ChangeDoChildrenPost (stmt, add_decision_labels mk_label)
    | _ ->
      Cil.DoChildren
end

let compute mk_label ast =
  Visitor.visitFramacFile (visitor mk_label) ast

end)
