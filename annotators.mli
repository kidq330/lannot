type annotation = int * string * Cil_types.exp * Cil_types.location
type annotator

(** Module type of annotators *)
module type ANNOTATOR = sig
  val name : string
  val descr : string

  (** Insert IN PLACE the annotation on the given AST.
   *
   * In addition of the AST, it also takes a function as parameter.
   * This function is provided by Annotators, it makes a label statement
   * from a boolean condition and an "origin" location (e.g. the
   * if-then-else condition's location in the case of a MCC label).
   *) 
  val compute : (Cil_types.exp -> Cil_types.location -> Cil_types.stmt) -> Cil_types.file -> unit
end
module type ANNOTATOR_WITH_EXTRA_TAGS = sig
  val name : string
  val descr : string
  val compute : (extra:string list -> Cil_types.exp -> Cil_types.location -> Cil_types.stmt) -> Cil_types.file -> unit
end

module type S = sig
  val self : annotator
end

(**
 * Register an annotator
 *)
module Register (A : ANNOTATOR) : S

(**
 * Register an annotator that provides extra tags (for instance, mutator tags)
 * in addition of the annotator name
 *)
module RegisterWithExtraTags (A : ANNOTATOR_WITH_EXTRA_TAGS) : S

val annotate_with : ?acc:annotation list -> ?nextId:int ref -> annotator -> Cil_types.file -> annotation list
val annotate : string list -> Cil_types.file -> annotation list

val shouldInstrument : Cil_types.varinfo -> bool
