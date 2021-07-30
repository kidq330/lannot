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

(** Return True if the stmt list only contain 1 statement which is a Break *)
let is_break_only bstmts =
  List.length bstmts = 1 &&
  match (List.hd bstmts).skind with
  | Break _ -> true
  | _ -> false

(** Add a label in each loop, to see if we enter in it *)
let inner mk_label = object(_)
  inherit Visitor.frama_c_inplace

  (** Pile qui stock le status des boucles *)
  val inLoopId = Stack.create ()

  method! vfunc dec =
    if Annotators.shouldInstrumentFun dec.svar then
      Cil.DoChildren
    else
      Cil.SkipChildren

  method! vstmt_aux stmt =
    match stmt.skind with
    | Loop (_,_,l,_,_) ->
      let label = mk_label (Exp_builder.one()) [] l in
      Cil.DoChildrenPost (fun stmt ->
          begin
            match stmt.skind with
            | Loop (_,b,_,_,_) ->
              let found = ref false in
              (* Cf. doc/loops.markdown for more info *)
              let len = List.length b.bstmts in
              let f id s =
                match s.skind with
                | If (e,th,el,l) ->
                  (* If not found already && then is empty && else contains only break &&
                     (isn't the last stmt in the body, or we don't want to handle do..while..) *)
                  if not !found && th.bstmts = [] && is_break_only el.bstmts && (id+1 <> len || not (Options.HandleDoWhile.get())) then begin
                    s.skind <- (If (e,{th with bstmts = [label]},el,l));
                    found := true
                  end;
                  s
                | _ -> s
              in
              let nb = List.mapi f b.bstmts in
              if !found then
                b.bstmts <- nb
              else
                b.bstmts <- label :: b.bstmts;
              stmt
            | _ -> assert false
          end
        )
    | _ ->
      Cil.DoChildren
end

(** Add a label in each loop, to see if we enter in it *)
let outter mk_label = object(self)
  inherit Visitor.frama_c_inplace

  (** Pile qui stock le status des boucles *)
  val inLoopId = Stack.create ()

  method private mk_comp ?(loc=Cil_datatype.Location.unknown) vi value =
    let new_exp = Exp_builder.mk ~loc (Lval (Var vi, NoOffset)) in
    Exp_builder.binop Eq new_exp value

  method private mkSeq loc =
    let id_seq = Annotators.getCurrentLabelId () + 1 in (* sequence id *)
    let vInfo = Cil.makeVarinfo false false
        ("__SEQ_STATUS_"^ string_of_int id_seq) Cil.intType
    in
    let use = self#mk_comp vInfo (Exp_builder.one ()) in
    let lbl_use = mk_label use [] loc in
    let def =
      Local_init(vInfo,AssignInit(SingleInit(Exp_builder.one ())),loc)
      |> Stmt_builder.instr
    in
    let cond =
      Ast_info.mkassign (Var vInfo, NoOffset) (Exp_builder.zero ()) loc
      |> Stmt_builder.instr
    in
    def,lbl_use,cond

  method! vfunc dec =
    if Annotators.shouldInstrumentFun dec.svar then
      Cil.DoChildren
    else
      Cil.SkipChildren

  method! vstmt_aux stmt =
    match stmt.skind with
    | Loop _ ->
      let def,use,cond = self#mkSeq (Cil_datatype.Stmt.loc stmt) in
      Cil.DoChildrenPost (fun stmt ->
          let newLoop =
            match stmt.skind with
            | Loop (_,b,_,_,_) ->
              let found = ref false in
              (* Cf. doc/loops.markdown for more info *)
              let len = List.length b.bstmts in
              let f id s =
                match s.skind with
                | If (e,th,el,l) ->
                  (* If not found already && then is empty && else contains only break &&
                     (isn't the last stmt in the body, or we don't want to handle do..while..) *)
                  if not !found && th.bstmts = [] && is_break_only el.bstmts
                     && (id+1 <> len || not (Options.HandleDoWhile.get ())) then begin
                    s.skind <- (If (e,{th with bstmts = [cond]},el,l));
                    found := true
                  end;
                  s
                | _ -> s
              in
              let nb = List.mapi f b.bstmts in
              if !found then
                b.bstmts <- nb
              else
                b.bstmts <- cond :: b.bstmts;
              stmt
            | _ -> assert false
          in
          Stmt_builder.block [def;newLoop;use]
        )
    | _ ->
      Cil.DoChildren
end

let warning () =
  if Options.HandleDoWhile.get () then
    Options.warning "Handle Do While enabled : Empty loops will be considered as Do..While"
  else
    Options.warning "Handle Do While disabled : Do..While will be considered as Empty loops";

include Annotators.Register (struct

    let name = "ELO"
    let help = "Enter in Loop at least once Coverage"

    let apply mk_label ast =
      warning();
      Visitor.visitFramacFileSameGlobals (inner mk_label) ast
  end)

include Annotators.Register (struct

    let name = "SLO"
    let help = "Skip Loop at least Once Coverage"

    let apply mk_label ast =
      warning();
      Visitor.visitFramacFileSameGlobals (outter mk_label) ast
  end)
