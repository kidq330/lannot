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

open Cil_types
open Annotators

include Register (struct

    let name = "FC"
    let help = "Function Coverage"

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

    let apply f ast =
      Visitor.visitFramacFileSameGlobals (visitor f) ast

  end)
