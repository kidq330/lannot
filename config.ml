open Cil_types
exception NoInputFile


(* val input_file: unit -> string *)
let input_file() =
  try
    let file = List.hd (Kernel.Files.get()) in
    let base = Filename.chop_extension (Filename.basename file) in
    base
  with
    | _ -> raise NoInputFile


(* val src_dir: unit -> string *)
let src_dir() =
  try
    let file = List.hd (Kernel.Files.get()) in
    Filename.dirname file
  with
    | _ -> raise NoInputFile
