open Cil_types

module Exp : sig
  val mk : ?loc:location -> exp_node -> exp

  (** [int] zero *)
  val zero : ?loc:location -> unit -> exp

  (** [int] one *)
  val one : ?loc:location -> unit -> exp

  (** [int] constant *)
  val integer : ?loc:location -> int -> exp

  val var : ?loc:location -> varinfo -> exp

  val lval : ?loc:location -> lval -> exp

  val mem : ?loc:location -> addr:exp -> off:offset -> exp

  (** Logical not *)
  val lnot : ?loc:location -> exp -> exp

  (** Arithmetic negation *)
  val neg : ?loc:location -> exp -> exp

  (** Binary operation *)
  val binop : ?loc:location -> binop -> exp -> exp -> exp

  (** Replace some subexpression by another (== equality) *)
  val replace : whole:exp -> part:exp -> repl:exp -> exp

  (** Joins some expressions (at least one) with a binary operator. *)
  val join : ?loc:location -> binop -> exp list -> exp

  val copy : exp -> exp
end

module Lval : sig
  val var : varinfo -> lval
  val mem : addr:exp -> off:offset -> lval
  val addOffset: off:offset -> base:lval -> lval
end

module Stmt : sig
  val mk : ?ghost:bool -> ?valid_sid:bool -> stmtkind -> stmt
  (** Make a block statement from a list of statements. *)
  val block : stmt list -> stmt
end

module Block : sig
  val mk : stmt list -> block
end
