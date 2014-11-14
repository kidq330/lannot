open Cil_types

(**
   Partitions the domain of some l-value (as a function) given its type

   The l-value is specified through a function that must returns a fresh copy
   each time (with unique expression ids).

   [depth] is the maximal depth to go into the l-value and [width] is the maximal
   width for array and structures explorations.
*)
val partition_lval : depth:int -> width:int -> emit:(exp -> unit) -> typ -> (unit -> lval) -> unit

(**
   Partitions the domain of some expression (as a function) given its type.

   See {!partition_lval} for details.
*)
val partition_exp : depth:int -> width:int -> emit:(exp -> unit) -> typ -> (unit -> exp) -> unit

module Partition : Annotators.S
