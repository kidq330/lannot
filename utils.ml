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

open Lexing
open Cil_types

(* for option slicing = NONE *)
let all_stmts = ref ([]:stmt list)

(* val get_stmt_loc: stmt -> loc *)
let get_stmt_loc = Cil_datatype.Stmt.loc

(* val get_stmt_loc_int: stmt -> int *)
let get_stmt_loc_int s = (fst (get_stmt_loc s)).pos_lnum

(* val same_line: stmt -> stmt -> bool *)
let same_line s1 s2 = (get_stmt_loc_int s1) = (get_stmt_loc_int s2)

(* val mk_call: ?loc:location -> ?result:lval -> string -> exp list -> stmt *)
let mk_call ?(loc=Cil_datatype.Location.unknown) ?result fname args =
  let new_lval loc v = Cil.new_exp loc (Lval (Cil.var v)) in
  let t = match result with
    | Some (Var v, _) -> v.vtype
    | _ -> Cil.voidType in
  let ty = TFun(t, None, false, []) in
  let f = new_lval loc (Cil.makeGlobalVar fname ty) in
  Cil.mkStmt ~valid_sid:true (Instr (Call (result, f, args, loc)))

(* val mk_exp: ?loc:location -> exp_node -> exp *)
let mk_exp ?(loc=Cil_datatype.Location.unknown) enode =
  Cil.new_exp loc enode

(* val mkdir: string -> unit *)
let mkdir x =
  if not (Sys.file_exists x) then
    Unix.mkdir x 0o744
