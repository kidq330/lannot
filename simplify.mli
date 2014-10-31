
type formula =
  | TAnd of formula * formula
  | TOr of formula * formula
  | TNot of formula
  | TAtom of int
  | TTrue
  | TFalse

val simplify : int -> formula -> formula

module type BOOLEAN_CONVERTIBLE = sig
  type t
  type info

  val convert : ?info:info -> t -> int*info*formula
  val convert_back : info:info -> formula -> t
end

module Make (C : BOOLEAN_CONVERTIBLE) : sig
  val simplify : C.t -> C.t
end

val simplify_exp : Cil_types.exp -> Cil_types.exp
