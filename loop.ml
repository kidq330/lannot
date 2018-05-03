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
open Ast_const

include Annotators.Register (struct

    let name = "LOOP"
    let help = "Loop Coverage"

    (** A visitor that adds a label at the start of each loop *)
    let visitor mk_label = object(_)
      inherit Visitor.frama_c_inplace

      val mutable first = false
      val lbl = Stack.create ()
      val mutable id = 0
      method! vfunc dec =
        if Annotators.shouldInstrument dec.svar then
          Cil.DoChildren
        else
          Cil.SkipChildren

      method! vstmt_aux stmt =
        match stmt.skind with
        | If (e,_,_,_) ->
          if first then begin
            let l = mk_label (Exp.lnot (Exp.copy e)) [] Cil_datatype.Location.unknown in
            Stack.push l lbl;
            first <- false
          end;
          Cil.DoChildren
        | Loop _ ->
          first <- true;
          Cil.DoChildrenPost (fun stmt ->
              try
                Stmt.block [Stack.pop lbl;stmt]
              with
                Stack.Empty -> stmt
            )
         | _ ->
           Cil.DoChildren
    end

    let apply f ast =
      Visitor.visitFramacFileSameGlobals (visitor f) ast

  end)
