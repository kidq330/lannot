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
    let help = "generate labels for each criterion (comma-separated \
                list of criteria, among "^string_list annotators^")"
  end)
let () = Annotators.set_possible_values annotators
let () = Annotators.add_aliases ["-lannotate"]

module Output = EmptyString (struct
    let option_name = "-lannot-o"
    let arg_name = "file"
    let help = "set output file (default: add _labels before extension)"
  end)
let () = Annotators.add_aliases ["-lannot-output"]

module AllBoolExps = False (struct
    let option_name = "-lannot-allbool"
    let help = "indicates that in addition to branching condition, \
                all boolean expression should be taken into account \
                (for CC, n-CC, MCC and DC coverage)"
  end)

module N = Int (struct
    let option_name = "-lannot-n"
    let arg_name = "N"
    let help = "set the N parameter for N-wise Condition Coverage \
                (0 means MCC and 1 means CC)"
    let default = 2
  end)

module MaxWidth = Int (struct
    let option_name = "-lannot-maxwidth"
    let arg_name = "NUM"
    let help = "set the maximum number of elements to partition in arrays \
                and structures (default: 5)"
    let default = 5
  end)

module MaxDepth = Int (struct
    let option_name = "-lannot-maxdepth"
    let arg_name = "NUM"
    let help = "set the maximal depth to partition, i.e. the maximum number \
                of pointer indirections and field accesses (default: 5)"
    let default = 5
  end)

module AllFuns = False (struct
    let option_name = "-lannot-allfuns"
    let help = "if IPD is enabled, inputs for all functions should be treated \
                (not only main)"
  end)

module GlobalsAsInput = False (struct
    let option_name = "-lannot-globals"
    let help = "global variables should be considered as input \
                (disabled by default)"
  end)

let mutators = ["AOR"; "ROR"; "COR"; "ABS"]
module Mutators = FilledStringSet (struct
    let option_name = "-lannot-mutators"
    let arg_name = "mutators"
    let help = "select mutators for WM labelling (comma-separated list \
                of mutators among "^string_list mutators^", default: all)"
    let default = Datatype.String.Set.of_list mutators
  end)
let () = Mutators.set_possible_values mutators

let () = Parameter_customize.argument_is_function_name ()
module FunctionNames = StringSet (struct
    let arg_name = "funs"
    let option_name = "-lannot-functions"
    let help = "filter by function names (disabled by default)"
  end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.do_not_journalize ()
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.do_not_save ()
module AnnotatorsHelp = False (struct
    let option_name = "-lannot-criteria-help"
    let help = "show criteria help"
  end)

module Simplify = False (struct
    let option_name = "-lannot-simplify"
    let help = "enable the simplification of boolean expressions before annotations"
  end)
