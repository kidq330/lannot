(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's Lannotate plug-in.                 *)
(*                                                                        *)
(*  Copyright (C) 2012-2022                                               *)
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
(*  for more details (enclosed in the file LICENSE)                       *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Ast_const

class to_visibility pc_label = object(self)
  inherit Visitor.frama_c_inplace
  val mutable to_add_vInfo = []
  val mutable to_add_ret = None,[]
  val unk_loc = Cil_datatype.Location.unknown

  method! vvdec v =
    if v.vname = "__retres" then
      v.vname <- "__retres_hidden_from_pc_view";
    SkipChildren

  method private mk_init ?(loc=unk_loc) vi value =
    Local_init(vi,AssignInit(SingleInit(value)),loc)
    |> Stmt_builder.instr

  method private mk_comp ?(loc=unk_loc) op vi value =
    let new_exp = Cil.new_exp ~loc (Lval (Var vi, NoOffset)) in
    Exp_builder.binop op new_exp value

  method! vfunc _ =
    Cil.DoChildrenPost (fun f ->
        let inits =
          List.map (fun (vi,_,_) ->
              self#mk_init vi (Cil.zero ~loc:unk_loc)
            ) to_add_vInfo
        in
        to_add_vInfo <- [];
        to_add_ret <- None,[];
        f.sbody.bstmts <- inits @ f.sbody.bstmts;
        f
      )

  method! vblock _ =
    Cil.DoChildrenPost (fun block ->
        match to_add_ret with
        | None, _ | Some _, [] -> block
        | Some s, lbls ->
          let rec aux l acc =
            match l with
            | [] -> acc
            | s' :: t when Cil_datatype.Stmt.equal s s' ->
              s'.skind <- Block (Cil.mkBlock (lbls @ [Stmt_builder.mk s'.skind]));
              aux t (acc @ [s'])
            | s' :: t -> aux t (acc @ [s'])
          in
          block.bstmts <- aux block.bstmts [];
          block
      )

  method! vstmt_aux s =
    match s.skind with
    | Instr (Call (_, {enode=Lval (Var {vname=name}, NoOffset)},
                   [cond;{enode=Const (CInt64 (id, IInt, None))} as idExp; tagExp], loc))
      when String.equal name "pc_label" ->
      let vname = "__SEQ_VISIBILITY_" ^ (string_of_int (Integer.to_int_exn id)) in
      let pred_vInfo = Cil.makeVarinfo false false vname Cil.intType in
      let exp_vInfo = Exp_builder.lval ~loc (Var pred_vInfo, NoOffset) in
      let keep_covered = Exp_builder.binop ~loc LOr exp_vInfo cond in
      let set =  Ast_info.mkassign (Var pred_vInfo, NoOffset) keep_covered loc in
      to_add_vInfo <- to_add_vInfo @ [(pred_vInfo,idExp,tagExp)];
      s.skind <- Instr set;
      Cil.SkipChildren
    | Return (_,loc) ->
      let lbls =
        List.map (fun (vi,id,tag) ->
            let cond = self#mk_comp ~loc Ne vi (Cil.zero ~loc) in
            Utils.mk_call pc_label [cond; id; tag]
          ) to_add_vInfo
      in
      to_add_ret <- (Some s,lbls);
      Cil.SkipChildren
    | _ -> Cil.DoChildren

end

let to_visibility ast pc_label =
  Visitor.visitFramacFileSameGlobals
    (new to_visibility pc_label :> Visitor.frama_c_visitor) ast
