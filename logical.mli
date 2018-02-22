(**
   Annotators for code-based logical coverage
*)

(** Condition coverage, special case of n-CC for n=1 *)
module CC : Annotators.S

(** N-wise Condition Coverage *)
module NCC : Annotators.S

(** Multiple Condition Coverage, special case of n-CC for n=Inf (coded 0) *)
module MCC : Annotators.S

(** Decision Coverage *)
module DC : Annotators.S

(** General Active Clause Coverage (weakened MC/DC) *)
module GACC : Annotators.S

(** Correlated Active Clause Coverage annotator *)
module CACC : Annotators.S

(** Restricted Active Clause Coverage annotator *)
module RACC : Annotators.S

(** General Inactive Clause Coverage annotator *)
module GICC : Annotators.S
