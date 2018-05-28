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

module Annotators = String_set (struct
    let option_name = "-lannot"
    let arg_name = "criteria"
    let help = "generate labels for each criterion (comma-separated \
                list of criteria, see -lannot-list)"
  end)
let () = Annotators.add_aliases ["-lannotate"]

module Output = Empty_string (struct
    let option_name = "-lannot-o"
    let arg_name = "file"
    let help = "set output file (default: add _labels before extension)"
  end)
let () = Output.add_aliases ["-lannot-output"]

module Simplify = False (struct
    let option_name = "-lannot-simplify"
    let help = "enable the simplification of boolean expressions before annotations"
  end)

module FunctionNames = Kernel_function_set (struct
    let arg_name = "funs"
    let option_name = "-lannot-functions"
    let help = "filter by function names (disabled by default)"
  end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.do_not_journalize ()
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.do_not_save ()
module ListAnnotators = False (struct
    let option_name = "-lannot-list"
    let help = "show list of criteria"
  end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.do_not_journalize ()
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.do_not_save ()
module ListAnnotatorsIncomp = False (struct
    let option_name = "-lannot-incomp-list"
    let help = "show list of criteria incompatibility"
  end)

let crit_group = add_group "Criterion-specific options"

module AllBoolExps = False (struct
    let option_name = "-lannot-allbool"
    let help = "indicates that in addition to branching condition, \
                all boolean expression should be taken into account \
                (for CC, n-CC, MCC, DC, GACC and GICC coverage)"
  end)

let () = Parameter_customize.set_group crit_group
module N = Int (struct
    let option_name = "-lannot-n"
    let arg_name = "N"
    let help = "set the n parameter for n-CC (n-wise Condition Coverage) \
                (0 means MCC and 1 means CC)"
    let default = 2
  end)

let () = Parameter_customize.set_group crit_group
let mutators = ["AOR"; "ROR"; "COR"; "ABS"]
module Mutators = Filled_string_set (struct
    let option_name = "-lannot-mutators"
    let arg_name = "mutators"
    let help = "select mutators for WM labelling (comma-separated list \
                of mutators among "^string_list mutators^", default: all)." ^
               "Mutators prefixed with '-' are removed from the list"

    let default = Datatype.String.Set.of_list mutators
  end)

let ipd_group = add_group "Options for Input Domain Partionning (IPD)"

let () = Parameter_customize.set_group ipd_group
module MaxWidth = Int (struct
    let option_name = "-lannot-maxwidth"
    let arg_name = "NUM"
    let help = "set the maximum number of elements to partition in arrays \
                and structures (default: 5)"
    let default = 5
  end)

let () = Parameter_customize.set_group ipd_group
module MaxDepth = Int (struct
    let option_name = "-lannot-maxdepth"
    let arg_name = "NUM"
    let help = "set the maximal depth to partition, i.e. the maximum number \
                of pointer indirections and field accesses (default: 5)"
    let default = 5
  end)

let () = Parameter_customize.set_group ipd_group
module AllFuns = False (struct
    let option_name = "-lannot-allfuns"
    let help = "if IPD is enabled, inputs for all functions should be treated \
                (not only main)"
  end)

let () = Parameter_customize.set_group ipd_group
module GlobalsAsInput = False (struct
    let option_name = "-lannot-globals"
    let help = "global variables should be considered as input \
                (disabled by default)"
  end)

let () = Parameter_customize.set_group crit_group
module LimitGapFloat = Int (struct
    let option_name = "-lannot-limitgap-float"
    let arg_name = "NUM"
    let help = "Set the number of zero decimal...[0-4] (default 2 => 0.001) "
    let default = 2
  end)

module ConstantFoldingArray = False (struct
    let option_name ="-lannot-constant-folding-array"
    let help = "For dataflows : Apply constant folding on array's indexes. If the result is a integer, then the sequence will be created for this specific index, not for the entire array (false by default). Todo : struct?"
  end)
