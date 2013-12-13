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

