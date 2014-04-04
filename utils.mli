open Cil_types

val all_stmts : stmt list ref
val get_stmt_loc : stmt -> location
val get_stmt_loc_int : stmt -> int
val same_line : stmt -> stmt -> bool
val mk_call :
  ?loc:location ->
  ?result:lval -> string -> exp list -> stmt
val mk_exp : ?loc:location -> exp_node -> exp
val mkdir : string -> unit

(**
  Indicates whether an expression is boolean in itself.
  Used to detect boolean expression outside conditional statement
*)
val is_boolean: exp -> bool

(**
  Makes an expresion that links the given list of expressions with boolean ands.
*)
val andify : ?loc:location -> exp list -> exp

(**
  Get atomic conditons form a boolean expression.
*)
val atomic_conditions : exp -> exp list

(** [combine n l] computes the combinations of [n] elements from the list [l].

  Returns the combination in the order of the list [l] and in a depth-first manner.
  For instance, [combine 2 [1;2;3]] returns [[1;2];[1;3];[2;3]].
*)
val combine : int -> 'a list -> 'a list list

(**
  [rev_combine n l] computes the combinations of [n] elements from the list [l].

  Returns the combination in the opposite order of {!combine}.
*)
val rev_combine : int -> 'a list -> 'a list list

(**
  [sign_combine pos neg l] computes all sign combinations of a list of elements [l], given two sign functions [pos] and [neg].

  Preserves the original order of the list, i.e. each sublist is in the same order.

  For instance, [sign_combine (fun x ->"+"^x) (fun x -> "-"^x) ["1";"2"]] returns [["+1";"+2"];["+1";"-2"];["-1";"+2"];["-1";"-2"]].
*)

val sign_combine : pos:('a -> 'b) -> neg:('a -> 'b) -> 'a list -> 'b list list

(**
  [sign_combine pos neg l] computes all sign combinations of a list of elements [l], given two sign functions [pos] and [neg].

  Returns the combination in the opposite order of {!sign_combine}.
*)
val rev_sign_combine : pos:('a -> 'b) -> neg:('a -> 'b) -> 'a list -> 'b list list


