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

module Exp_builder : sig
  val mk : ?loc:location -> exp_node -> exp

  (** [int] zero *)
  val zero : ?loc:location -> unit -> exp

  (** [int] one *)
  val one : ?loc:location -> unit -> exp

  (** [int] constant *)
  val integer : ?loc:location -> int -> exp

  (** [ikind] constant *)
  val kinteger : ?loc:location -> ikind -> int -> exp

  (** [ikind] constant *)
  val kinteger64 : ?loc:location -> ikind -> ?repr:string -> Integer.t -> exp

  (** [float] constant *)
  val float : ?loc:location -> float -> exp

  (** [string] constant *)
  val string : ?loc:location -> string -> exp

  val var : ?loc:location -> varinfo -> exp

  val lval : ?loc:location -> lval -> exp

  val mem : ?loc:location -> exp -> offset -> exp

  (** Logical not *)
  val lnot : ?loc:location -> exp -> exp

  (** Arithmetic negation *)
  val neg : ?loc:location -> exp -> exp

  (** Binary operation *)
  val binop : ?loc:location -> binop -> exp -> exp -> exp

  (** Implies *)
  val implies : ?loc:location -> exp -> exp -> exp

  (** Iff *)
  val iff : ?loc:location -> exp -> exp -> exp

  (** Xor, boolean disequality *)
  val niff : ?loc:location -> exp -> exp -> exp

  (** Replace some subexpression by another (== equality) *)
  val replace : whole:exp -> part:exp -> repl:exp -> exp

  (** Joins some expressions (at least one) with a binary operator. *)
  val join : ?loc:location -> binop -> exp list -> exp
  val rev_join : ?loc:location -> binop -> exp list -> exp
end

module Stmt_builder : sig
  val mk : ?ghost:bool -> ?sattr:attributes  -> stmtkind -> stmt
  val instr : ?ghost:bool -> ?sattr:attributes  -> instr -> stmt

  (** Make a block statement from a list of statements. *)
  val block : stmt list -> stmt
end
