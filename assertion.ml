(**
   Assert coverage annotator
*)
module AssertCov = Annotators.Register (struct
    let name = "ASSERT"
    let help = "Assert Coverage"
    let apply _mk_label _file =
      let ext  = Dynamic.get ~plugin:"stady" "run_stady_labels" (Datatype.func Datatype.unit Datatype.unit) in
      ext ()
  end)
