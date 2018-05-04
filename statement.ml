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

let unk_loc = Cil_datatype.Location.unknown

include Annotators.Register (struct

    let name = "STMT"
    let help = "Statement Coverage"

    (** A visitor that adds a label at the start of each statement *)
    let visitor mk_label = object(self)
      inherit Visitor.frama_c_inplace

      method! vfunc dec =
        if Annotators.shouldInstrument dec.svar then
          let l = mk_label (Exp.one()) [] unk_loc in
          Cil.DoChildrenPost (fun res ->
              res.sbody.bstmts <- l :: res.sbody.bstmts;
              res
            )
        else
          Cil.SkipChildren

      method! vstmt_aux stmt =
        let lbl = List.length stmt.labels != 0 in
        match stmt.skind with
         | Goto (_, _)
         | Return (_, _) ->
           let l = mk_label (Exp.one()) [] unk_loc in
           if not lbl then
             Cil.ChangeTo (Stmt.block ([l;stmt]))
           else begin
             stmt.skind <- Block (Cil.mkBlock (l :: [Cil.mkStmt stmt.skind]));
             Cil.ChangeTo stmt
           end
         | If (e,b1,b2,l) ->
           let nb1 = Cil.visitCilBlock (self :> Cil.cilVisitor) b1 in
           let nb2 = Cil.visitCilBlock (self :> Cil.cilVisitor) b2 in
           let l1 = mk_label (Exp.one()) [] unk_loc in
           let l2 = mk_label (Exp.one()) [] unk_loc in
           nb1.bstmts <- l1 :: nb1.bstmts;
           nb2.bstmts <- l2 :: nb2.bstmts;
           stmt.skind <- (If (e,nb1,nb2,l));
           Cil.ChangeTo stmt
         | Loop _ ->
           Cil.DoChildrenPost (fun res ->
               match res.skind with
               | Loop  (ca, b, s1, s2, l) ->
                 let lb = mk_label (Exp.one()) [] unk_loc in
                 b.bstmts<- lb :: b.bstmts;
                 res.skind <- (Loop (ca,b,s1,s2,l));
                 res
               | _ -> assert false
             )
         | _ ->
           Cil.DoChildrenPost (fun res ->
               if lbl then begin
                 let lb = mk_label (Exp.one()) [] unk_loc in
                 {res with skind = Block (Cil.mkBlock (lb :: [Cil.mkStmt res.skind]))}
               end
               else
                 res
             )
    end

    let apply f ast =
      Visitor.visitFramacFileSameGlobals (visitor f) ast

  end)
