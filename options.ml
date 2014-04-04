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

include Plugin.Register (struct
  let name = "LAnnotate"
  let shortname = "lannot"
  let help = "generate labels"
end)

let rec string_list l =
  match l with
  | [] -> ""
  | a :: [] -> a
  | a :: b :: [] -> a^" and "^b
  | head :: tail -> head^", "^string_list tail

let annotators = ["MCC"; "CC"; "FC"; "DC"; "WM"; "IDP"]
module Annotators = StringSet (struct
  let option_name = "-lannot"
  let arg_name = "criteria"
  let help = "generate labels for each criterion (comma-separated list of criteria, among "^string_list annotators^")"
end)
let () = Annotators.set_possible_values annotators
let () = Annotators.add_aliases ["-lannotate"]

let () = Parameter_customize.do_not_projectify () (* !!! Allbools does not seem to stick without it*)
module AllBoolExps = False (struct
  let option_name = "-lannot-allbool"
  let help = "indicates that in addition to branching condition, all boolean expression should be taken into account (*CC, DC for coverage)"
end)
let () = AllBoolExps.add_update_hook (fun oldb newb -> debug "allbool change %B %B" oldb newb)

module N = Int (struct
  let option_name = "-lannot-n"
  let arg_name = "N"
  let help = "set the N parameter (NCC: 0 means MCC and 1 means CC)"
  let default = 2
end)
let () = N.add_update_hook (fun oldb newb -> debug "N change %d %d" oldb newb)


let mutators = ["AOR"; "ROR"; "COR"; "ABS"]
module Mutators = FilledStringSet (struct
  let option_name = "-lannot-mutators"
  let arg_name = "mutators"
  let help = "select mutators for WM labelling (comma-separated list of mutators among "^string_list mutators^", default: all)"
  let default = Datatype.String.Set.of_list mutators
end)
let () = Mutators.set_possible_values mutators

let () = Parameter_customize.argument_is_function_name ()
module FunctionNames = StringSet (struct
  let arg_name = "funs"
  let option_name = "-lannot-functions"
  let help = "filter by function names (by default: disabled)"
end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.do_not_journalize ()
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.do_not_save ()
module AnnotatorsHelp = False (struct
  let option_name = "-lannot-criteria-help"
  let help = "show criteria help"
end)

