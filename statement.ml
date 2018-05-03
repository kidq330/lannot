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

    let name = "STMT"
    let help = "Statement Coverage"

    (** A visitor that adds a label at the start of each statement *)
    let visitor mk_label = object(self)
      inherit Visitor.frama_c_inplace

      method! vfunc dec =
        if Annotators.shouldInstrument dec.svar then
          let l = mk_label (Cil.one Cil_datatype.Location.unknown) [] Cil_datatype.Location.unknown in
          Cil.DoChildrenPost (fun res ->
              res.sbody.bstmts <- l :: res.sbody.bstmts;
              res
            )
        else
          Cil.SkipChildren

      method! vstmt_aux stmt =
        let lbl = List.length stmt.labels != 0 in
        match stmt.skind with
         | Goto (_, _) ->
           let l = mk_label (Cil.one Cil_datatype.Location.unknown) [] Cil_datatype.Location.unknown in
           if not lbl then
             Cil.ChangeTo (Stmt.block ([l;stmt]))
           else
             Cil.ChangeTo ({stmt with skind = Block (Cil.mkBlock (l :: [Cil.mkStmt stmt.skind]))})
         | Return (_, _) ->
           let l = mk_label (Cil.one Cil_datatype.Location.unknown) [] Cil_datatype.Location.unknown in
           if not lbl then
             Cil.ChangeTo (Stmt.block ([l;stmt]))
           else
             Cil.ChangeTo ({stmt with skind = Block (Cil.mkBlock (l :: [Cil.mkStmt stmt.skind]))})
         | If(e,b1,b2,l) ->
           let nb1 = Cil.visitCilBlock (self :> Cil.cilVisitor) b1 in
           let nb2 = Cil.visitCilBlock (self :> Cil.cilVisitor) b2 in
           let l1 = mk_label (Cil.one Cil_datatype.Location.unknown) [] Cil_datatype.Location.unknown in
           let l2 = mk_label (Cil.one Cil_datatype.Location.unknown) [] Cil_datatype.Location.unknown in
           nb1.bstmts <- l1 :: nb1.bstmts;
           nb2.bstmts <- l2 :: nb2.bstmts;
           Cil.ChangeTo (Cil.mkStmt (If (e,nb1,nb2,l)))
         | Switch (e,b,s,l) ->
           let nb = Cil.visitCilBlock (self :> Cil.cilVisitor) b in
           Cil.ChangeTo (Cil.mkStmt (Switch (e,nb,s,l)))
         | Loop _ ->
           Cil.DoChildrenPost (fun res ->
               match res.skind with
               | Loop  (ca, b, s1, s2, l) ->
                 let lb = mk_label (Cil.one Cil_datatype.Location.unknown) [] Cil_datatype.Location.unknown in
                 b.bstmts<- lb :: b.bstmts;
                 (Cil.mkStmt (Loop (ca,b,s1,s2,l)))
               | _ -> assert false
             )
         | _ ->
           Cil.DoChildrenPost (fun res ->
               if lbl then begin
                 let lb = mk_label (Cil.one Cil_datatype.Location.unknown) [] Cil_datatype.Location.unknown in
                 {res with skind = Block (Cil.mkBlock (lb :: [Cil.mkStmt res.skind]))}
               end
               else
                 res
             )
    end

    let apply f ast =
      Visitor.visitFramacFileSameGlobals (visitor f) ast

  end)
