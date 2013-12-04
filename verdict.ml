exception InconsistentVerdict

type t = Bug of string | Safe | Unknown

(* val string_of: t -> string *)
let string_of = function
  | Bug s -> "Bug : " ^ s
  | Safe -> "Safe"
  | Unknown -> "Unknown"


(* val merge: t -> t -> t *)
let merge a b =
  match (a,b) with
    | Bug _, Safe | Safe, Bug _ -> raise InconsistentVerdict
    | Unknown, a | a, _ -> a
