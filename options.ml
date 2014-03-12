include Plugin.Register (struct
  let name = "GenLabels"
  let shortname = "genlabels"
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
  let option_name = "-genlabels"
  let arg_name = "criteria"
  let help = "generate labels for each criterion (comma-separated list of criteria, among "^string_list annotators^")"
end)
let () = Annotators.set_possible_values annotators

let mutators = ["AOR"; "ROR"; "COR"; "ABS"]
module Mutators = FilledStringSet (struct
  let option_name = "-genlabels-mutators"
  let arg_name = "mutators"
  let help = "select mutators for WM labelling (comma-separated list of mutators among "^string_list mutators^", default: all)"
  let default = Datatype.String.Set.of_list mutators
end)
let () = Mutators.set_possible_values mutators

let () = Plugin.argument_is_function_name ()
module FunctionNames = StringSet (struct
  let arg_name = "funs"
  let option_name = "-genlabels-functions"
  let help = "filter by function names (by default: disabled)"
end)

let () = Plugin.set_group help
let () = Plugin.do_not_journalize ()
module AnnotatorsHelp = False (struct
  let option_name = "-genlabels-criteria-help"
  let help = "show criteria help"
end)

