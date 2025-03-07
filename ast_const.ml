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

let unk_loc = Cil_datatype.Location.unknown

module Exp_builder = struct
  let mk ?(loc=unk_loc) =
    Cil.new_exp ~loc

  let zero ?(loc=unk_loc) () =
    Cil.zero ~loc

  let one ?(loc=unk_loc) () =
    Cil.one ~loc

  let integer ?(loc=unk_loc) =
    Cil.integer ~loc

  let kinteger ?(loc=unk_loc) =
    Cil.kinteger ~loc

  let kinteger64 ?(loc=unk_loc) kind =
    Cil.kinteger64 ~loc ~kind

  let float ?(loc=unk_loc) =
    Cil.kfloat ~loc FDouble

  let string ?(loc=unk_loc) =
    Cil.mkString ~loc

  let var ?(loc=unk_loc) varinfo =
    Cil.evar ~loc varinfo

  let lval ?(loc=Cil_datatype.Location.unknown) lval =
    Cil.new_exp ~loc (Lval lval)

  let mem ?(loc=unk_loc) addr off =
    Cil.new_exp ~loc (Lval (Cil.mkMem ~addr ~off))

  let lnot ?(loc=unk_loc) e =
    Cil.new_exp ~loc (UnOp (LNot, e, Cil.intType))

  let neg ?(loc=unk_loc) e =
    let oldt = Cil.typeOf e in
    let newt = Cil.integralPromotion oldt in
    (* make integral promotion explicit *)
    let e' = Cil.mkCastT ~oldt ~newt e in
    Cil.new_exp ~loc (UnOp (Neg, e', newt))

  let binop ?(loc=unk_loc) op left right =
    Cil.mkBinOp ~loc op left right

  let implies ?(loc=unk_loc) l r =
    let n_l = lnot ~loc l in
    Cil.mkBinOp ~loc LOr n_l r

  let iff ?(loc=unk_loc) l r =
    let l_imp_r = implies ~loc l r in
    let r_imp_l = implies ~loc r l in
    Cil.mkBinOp ~loc LAnd l_imp_r r_imp_l

  let niff ?(loc=unk_loc) l r =
    let nl_and_r = Cil.mkBinOp ~loc LAnd (lnot ~loc l) r in
    let l_and_nr = Cil.mkBinOp ~loc LAnd l (lnot ~loc r) in
    Cil.mkBinOp ~loc LOr nl_and_r l_and_nr

  let rec replace ~whole ~part ~(repl: exp) : exp=
    if part == whole then repl
    else
      match whole.enode with
      | UnOp (op, e, typ) ->
        let e' = replace ~whole:e ~part ~repl in
        if e == e' then whole else mk ~loc:whole.eloc (UnOp (op, e', typ))
      | BinOp (op, e1, e2, typ) ->
        let e1' = replace ~whole:e1 ~part ~repl in
        let e2' = replace ~whole:e2 ~part ~repl in
        if e1 == e1' && e2 == e2' then whole else mk ~loc:whole.eloc (BinOp (op, e1', e2', typ))
      | _ -> whole

  (** Joins some expressions (at least one) with a binary operator. *)
  let rev_join ?(loc=Cil_datatype.Location.unknown) op l =
    match l with
    | [] -> invalid_arg "join"
    | head :: tail ->
      List.fold_left (fun acc e -> Cil.mkBinOp ~loc op e acc) head tail

  let join ?(loc=Cil_datatype.Location.unknown) op l =
    rev_join ~loc op (List.rev l)
end

module Stmt_builder = struct
  let mk = Cil.mkStmt ~valid_sid:true
  let instr = Cil.mkStmtOneInstr ~valid_sid:true
  let block stmts = mk (Block (Cil.mkBlock stmts))
end
