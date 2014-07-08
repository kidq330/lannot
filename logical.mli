(**
 * Annotators for code-based logical coverage
 *)

(** Condition coverage, special case of n-CC for n=1 *)
module CC : Annotators.S

(** N-wise Condition Coverage *)
module NCC : Annotators.S

(** Multiple Condition Coverage, special case of n-CC for n=Inf (coded 0) *)
module MCC : Annotators.S

(** Decision coverage *)
module DC : Annotators.S

(** General Active Clause Coverage (weakened MC/DC) *)
module GACC : Annotators.S
