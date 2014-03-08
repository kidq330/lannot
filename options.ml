include Plugin.Register (struct
  let name = "GenLabels"
  let shortname = "genlabels"
  let help = "generate labels"
end)

module Enabled = False (struct
  let option_name = "-genlabels"
  let help = "generate labels"
end)

module SimpleCond = False (struct
  let option_name = "-genlabels-simple"
  let help = "generate labels with simple conditions"
end)

module MultiCond = False (struct
  let option_name = "-genlabels-multi"
  let help = "generate labels with multiple conditions"
end)

module AOR = False (struct
  let option_name = "-genlabels-aor"
  let help = "generate mutants based on the AOR criteria (Arithmetic Operator Replacement)"
end)

module ROR = False (struct
  let option_name = "-genlabels-ror"
  let help = "generate mutants based on the ROR criteria"
end)

module COR = False (struct
  let option_name = "-genlabels-cor"
  let help = "generate mutants based on the COR criteria"
end)

module ABS = False (struct
  let option_name = "-genlabels-abs"
  let help = "generate mutants based on the ABS criteria"
end)

module PARTITION = False (struct
  let option_name = "-genlabels-partition"
  let help = "generate labels partitioning input"
end)

module FunctionNames = StringSet (struct
  let arg_name = "FUNS"
  let option_name = "-genlabels-filter"
  let help = "filter by function names (by default: disabled)"
end)
