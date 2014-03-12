open Cil_types
open Annotators

include Register (struct

let name = "FC"
let descr = "Function Coverage"

(** A visitor that adds a label at the start of each function's body *)
let visitor mk_label = object
  inherit Visitor.frama_c_inplace

  method! vfunc dec =
    if shouldInstrument dec.svar then begin
      let label = mk_label (Cil.one Cil_datatype.Location.unknown) dec.svar.vdecl in
      dec.sbody.bstmts <- label :: dec.sbody.bstmts;
    end;
    Cil.SkipChildren
end

let compute f ast =
  Visitor.visitFramacFileSameGlobals (visitor f) ast

end)
