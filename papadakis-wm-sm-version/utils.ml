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

let formatter = ref (Format.formatter_of_out_channel (stdout))

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

let isSameType typ1 typ2 = (*consider pointers*)
if Cil.isArithmeticType typ1 then begin
  if Cil.isArithmeticType typ2 then true
  else false
end
else if Cil.isCharType typ1 then begin
  if Cil.isCharType typ2 then true
  else false
end
else begin 
let enumtype typ = match typ with | TEnum(info, att) -> true | _ -> false in 
if enumtype typ1 then begin
  if enumtype typ2 then true
  else false
  end
else false
end

let rec isdivisionSafe currExp candidateMutExp = 
    if currExp.eid = candidateMutExp.eid then begin true end else begin
 match currExp.enode with
          | BinOp(Div, lexp, rexp, ty) -> if lexp.eid = candidateMutExp.eid then begin true end else begin false end 
          | BinOp(Mod, lexp, rexp, ty) -> if lexp.eid = candidateMutExp.eid then begin true end else begin false end 
          | BinOp(op, lexp, rexp, ty) -> isdivisionSafe lexp candidateMutExp ; isdivisionSafe rexp candidateMutExp 
          | UnOp(op, exp, ty) -> isdivisionSafe exp candidateMutExp
          | _ -> false
end 

(*let rec isPointerRemoveSafe currExp candidateMutExp after = 
    if currExp.eid = candidateMutExp.eid then begin after := true; true end else if !after then checkafter currExp else begin 
 match currExp.enode with
          | BinOp(_, lexp, rexp, ty) -> isPointerRemoveSafe lexp candidateMutExp after; if !after then checkafter rexp else isPointerRemoveSafe rexp candidateMutExp after
          | UnOp(_, exp, ty) -> isPointerRemoveSafe exp candidateMutExp after
          | _ -> true
 end 
and
 checkafter exp = 
   let typ = Cil.typeOf exp in
   if ( Cil.isPointerType typ ) then 
     true
   else false
*)





