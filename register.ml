(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  You may redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful, but WITHOUT     *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General      *)
(*  Public License for more details.                                      *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1 for more        *)
(*  details (enclosed in the file LICENSE).                               *)
(*                                                                        *)
(**************************************************************************)

let store_label_data out annotations =
  (* TODO do that in its own module, ultimately shared with the other LTest-tools *)
  (* TODO (later) do something better than csv *)
  let formatter = Format.formatter_of_out_channel out in
  Format.fprintf formatter "# id, status, tags, origin, current@.";
  let print_one (id, tags, _cond, origin_loc) =
    let origin_file = (fst origin_loc).Lexing.pos_fname in
    let origin_line = (fst origin_loc).Lexing.pos_lnum in
    Format.fprintf formatter "%d, unknown, %s, %s:%d,@." id tags origin_file origin_line
  in
  List.iter print_one annotations;
  Format.pp_print_flush formatter ()


let annotate ann_names =
  Options.feedback "started";
  let prj_name = Config.input_file () in
  let prj = Project.create_by_copy prj_name in
  (* TODO use an options to set the output filename *)
  let filename = (Project.get_name prj) ^ "_labels.c" in
  let data_filename = (Project.get_name prj) ^ "_labels.labels" in
  Project.set_current prj;

  let annotations = Annotators.annotate ann_names (Ast.get ()) in

  (* output modified c file *)
  Options.feedback "write modified C file";
  let out = open_out filename in
  let formatter = Format.formatter_of_out_channel out in
  File.pretty_ast ~prj:prj ~fmt:formatter ();
  Format.pp_print_flush formatter ();
  close_out out;

  (* output label data *)
  Options.feedback "write label data";
  let out = open_out data_filename in
  store_label_data out annotations;
  close_out out;
  Options.feedback "finished"

let setupMutatorOptions () =
  let f mutname =
    if mutname = "AOR" then Instru.aorOption := true
    else if mutname = "COR" then Instru.corOption := true
    else if mutname = "ABS" then Instru.absOption := true
    else if mutname = "ROR" then Instru.rorOption := true
  in
  Options.Mutators.iter f

(* ENTRY POINT *)
let run () =
  try
    setupMutatorOptions ();
    annotate (Datatype.String.Set.elements (Options.Annotators.get ()))
  with
    | Globals.No_such_entry_point _ ->
	Options.feedback "`-main` parameter missing"
    | Dynamic.Unbound_value(s) -> Options.feedback "%s unbound" s
    | Dynamic.Incompatible_type(s) -> Options.feedback "%s incompatible" s
    | Config.NoInputFile -> Options.feedback "no input file"
    | Failure s -> Options.feedback "failure: %s" s
    | e -> Options.feedback "exception: %s" (Printexc.to_string e)
	
let run () =
  if Options.AnnotatorsHelp.get () then
  begin
    Annotators.print_help Format.std_formatter;
    exit 0;
  end
  else if not (Options.Annotators.is_empty ()) then
    let deps = [Ast.self] in
    let f, _self = State_builder.apply_once "GENLABELS" deps run in
      Kernel.LogicalOperators.on ();
      f ();
      Kernel.LogicalOperators.off ()

let () =
  Db.Main.extend run

