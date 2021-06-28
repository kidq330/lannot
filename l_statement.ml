(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2020                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Ast_const

let unk_loc = Cil_datatype.Location.unknown

(* Est-ce qu'on peut recup' la loc plus simplement? *)
let get_fundec_loc dec =
  let glob = Ast.def_or_last_decl dec.svar in
  match glob with
  | GFun (_, loc) -> loc
  | _ -> unk_loc

(** A visitor that adds a label at the start of each statement
    i.e. :
    -Start of a function
    -Goto/Return
    -Each block of a If
    -In Loops
    -After each C labels

*)
let visitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  method! vfunc dec =
    if Annotators.shouldInstrumentFun dec.svar then
      let l = mk_label (Exp_builder.one()) [] (get_fundec_loc dec) in
      Cil.DoChildrenPost (fun res ->
          res.sbody.bstmts <- l :: res.sbody.bstmts;
          res
        )
    else
      Cil.SkipChildren

  method! vstmt_aux stmt =
    match stmt.skind with
    | Goto (_, loc)
    | Return (_, loc) ->
      let l = mk_label (Exp_builder.one()) [] loc in
      stmt.skind <- Block (Cil.mkBlock [l; Stmt_builder.mk stmt.skind]);
      Cil.SkipChildren
    | If (e,b1,b2,loc) ->
      let l1 = mk_label (Exp_builder.one()) [] loc in
      let nb1 = Cil.visitCilBlock (self :> Cil.cilVisitor) b1 in
      let l2 = mk_label (Exp_builder.one()) [] loc in
      let nb2 = Cil.visitCilBlock (self :> Cil.cilVisitor) b2 in
      nb1.bstmts <- l1 :: nb1.bstmts;
      nb2.bstmts <- l2 :: nb2.bstmts;
      stmt.skind <- (If (e,nb1,nb2,loc));
      Cil.SkipChildren
    | Loop _ ->
      Cil.DoChildrenPost (fun res ->
          match res.skind with
          | Loop  (ca, b, loc, s1, s2) ->
            let lb = mk_label (Exp_builder.one()) [] loc in
            b.bstmts<- lb :: b.bstmts;
            res.skind <- (Loop (ca,b,loc,s1,s2));
            res
          | _ -> assert false
        )
    | _ ->
      Cil.DoChildrenPost (fun res ->
          if res.labels <> [] then begin
            let loc =
              match List.hd stmt.labels with
              | Label (_,l,_)
              | Case (_,l)
              | Default l -> l
            in
            let lb = mk_label (Exp_builder.one()) [] loc in
            res.skind <-
              begin match res.skind with
                | Instr(Skip _) -> lb.skind
                | _ -> Block (Cil.mkBlock [lb; Stmt_builder.mk res.skind]);
              end;
            res
          end
          else
            res
        )
end

include Annotators.Register (struct
    let name = "STMT"
    let help = "Statement Coverage"

    let apply f ast =
      Visitor.visitFramacFileSameGlobals (visitor f) ast

  end)
