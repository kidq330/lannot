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


(**
  Indicates whether an expression is boolean in itself.
  Used to detect boolean expression outside conditional statement
*)
let is_boolean e =
  Options.debug "is boolean? @[%a@]@." Cil_printer.pp_exp e;
  match e.enode with
  (* C99 _Bool type *)
  | BinOp (_, _, _, TInt (IBool, _))
  | UnOp (_, _, TInt (IBool, _))

  (* Use of logical operators or relational operators *)
  | BinOp ((LAnd|LOr|Lt|Gt|Le|Ge|Eq|Ne), _, _, _)
  | UnOp (LNot, _, _) -> true
  | _ -> false

(**
  Links a list of expressions with boolean ands.
*)
let andify ?(loc=Cil_datatype.Location.unknown) conj =
  match conj with
  | [] -> assert false
  | head :: tail ->
    List.fold_left (fun acc e -> Cil.mkBinOp loc LAnd acc e) head tail

(**
  Get atomic conditions from a boolean expression.
*)
let atomic_conditions =
  let rec aux acc exp =
    match exp.enode with
    | BinOp (Ne, e, zero, _)
    | BinOp (Ne, zero, e, _) when Cil.isZero zero && is_boolean e ->
      (* Cil adds !=0 when && or || are present in normal expression (don't do that for !)*)
      aux acc e
    | BinOp ((LAnd | LOr), e1, e2, _) ->
      aux (aux acc e1) e2
    | UnOp (LNot, e, _) ->
      aux acc e
    | _ ->
      exp :: acc
  in
  fun exp -> List.rev (aux [] exp)

let rev_combine : int -> 'a list -> 'a list list =
  let rec comb_aux rev_startswith acc n l =
    match n, l with
    | 0, _ -> (List.rev rev_startswith) :: acc
    | _, [] -> acc
    | _, head :: tail ->
      let acc = comb_aux (head :: rev_startswith) acc (n-1) tail in
      comb_aux rev_startswith acc n tail
  in
  fun n l -> comb_aux [] [] n l (*Needed to keep polymorphism, wtf!?*)

(** [combine n l] computes the combinations of [n] elements from the list [l].

  Returns the combination in the order of the list [l].
  For instance, [combine 2 [1;2;3]] returns [[1;2];[1;3];[2;3]].
*)
let combine n (l : 'a list) : 'a list list =
  List.rev (rev_combine n l)

(**
  [sign_combine pos neg l] computes all sign combinations of a list of elements [l], given two sign functions [pos] and [neg].

  Preserves the original order of the list, i.e. each sublist is in the same order.

  For instance, [sign_combine (fun x ->"+"^x) (fun x -> "-"^x) ["1";"2"]] returns [["+1";"+2"];["+1";"-2"];["-1";"+2"];["-1";"-2"]].
*)
let rev_sign_combine ~(pos: 'a -> 'b) ~(neg : 'a -> 'b) : 'a list -> 'b list list =
  let rec aux acc revpref l =
    match l with
    | [] -> (List.rev revpref) :: acc
    | head :: tail ->
      let acc = aux acc (pos head :: revpref) tail in
      aux acc (neg head :: revpref) tail
  in
  aux [] []

let sign_combine ~pos ~neg l =
  List.rev (rev_sign_combine pos neg l)

