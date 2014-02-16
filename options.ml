module Self = Plugin.Register (struct
  let name = "GenLabels"
  let shortname = "genlabels"
  let help = "generate labels"
end)

module Enabled = Self.False (struct
  let option_name = "-genlabels"
  let help = "generate labels"
end)

module MultiCond = Self.False (struct
  let option_name = "-genlabels-multi"
  let help = "generate labels with multiple conditions"
end)

module AOR = Self.False (struct
  let option_name = "-genlabels-aor"
  let help = "generate mutants based on the AOR criteria (Arithmetic Operator Replacement)"
end)

module ROR = Self.False (struct
  let option_name = "-genlabels-ror"
  let help = "generate mutants based on the ROR criteria (Arithmetic Operator Replacement)"
end)

