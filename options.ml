include Plugin.Register (struct
  let name = "GenLabels"
  let shortname = "genlabels"
  let help = "generate labels"
end)

module Annotators = StringSet (struct
  let option_name = "-genlabels"
  let arg_name = "CATEGORIES"
  let help = "generate labels for each criterion (comma-separated list of criteria, among MCC, CC, IDP, FC, DC, WM) "
end)
(* annotators.ml calls Annotators.set_possible_values *)

module Mutators = FilledStringSet (struct
  let option_name = "-genlabels-mutators"
  let arg_name = "MUTATORS"
  let help = "select mutators for WM-label annotation (comma-separated list of mutators among AOR, ROR, COR, ABS, default: all)"
  let default = Datatype.String.Set.of_list ["AOR"; "ROR"; "COR"; "ABS"] 
end)
(*TODO instru.ml should called set_possible_values with the mutator lists *)
let () = Mutators.set_possible_values ["AOR"; "ROR"; "COR"; "ABS"]

module FunctionNames = StringSet (struct
  let arg_name = "FUNS"
  let option_name = "-genlabels-functions"
  let help = "filter by function names (by default: disabled)"
end)

