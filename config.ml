(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C/LTest.                                   *)
(*                                                                        *)
(*  Copyright (C) 2013-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

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
