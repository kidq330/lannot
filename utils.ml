open Lexing
open Cil_types

(* for option slicing = NONE *)
let all_stmts = ref ([]:stmt list)

(* val get_stmt_loc: stmt -> loc *)
let rec get_stmt_loc = Cil_datatype.Stmt.loc
    
(* val get_stmt_loc_int: stmt -> int *)
let rec get_stmt_loc_int s = (fst (get_stmt_loc s)).pos_lnum

(* val same_line: stmt -> stmt -> bool *)
let same_line s1 s2 = (get_stmt_loc_int s1) = (get_stmt_loc_int s2)

(* val mkdir: string -> unit *)
let mkdir x =
  if not (Sys.file_exists x) then
    Unix.mkdir x 0o744
