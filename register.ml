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
  Format.fprintf formatter "# id, status, tags, origin, current, verdict emitter@.";
  let print_one (id, tags, cond, origin_loc) =
    let origin_file = (fst origin_loc).Lexing.pos_fname in
    let origin_line = (fst origin_loc).Lexing.pos_lnum in
    let verdict = if Cil.isZero cond then "uncoverable" else "unknown" in
    Format.fprintf formatter "%d,%s,%s,%s:%d,,lannot@." id verdict tags origin_file origin_line
  in
  List.iter print_one annotations;
  Format.pp_print_flush formatter ()

let compute_outfile opt files =
  if opt = "" then
    if files = [] then
      "a_labels.out"
    else
      let base = List.hd files in
      (* TODO check if it's not better to take the last filename *)
      let prefix = Filename.chop_extension base in
      let len_prefix = String.length prefix in
      let suffix = String.sub base len_prefix ((String.length base)-len_prefix) in
      prefix ^ "_labels" ^ suffix
  else
    opt;;

let annotate ann_names =
  let base_project = Project.current () in
  let prj_name = (Project.get_name base_project) ^ "_labels" in
  let prj = Project.create_by_copy prj_name in
  Project.set_current prj;
  Options.debug "start project %s" prj_name;

  let annotations = ref [] in
  let collect ann = annotations := ann :: !annotations in
  Annotators.annotate ~collect ann_names (Ast.get ());
  let annotations = !annotations in

  (* output modified c file *)
  let filename = compute_outfile (Options.Output.get ()) (Kernel.Files.get ()) in
  Options.feedback "write modified C file (to %s)" filename;
  let out = open_out filename in
  let formatter = Format.formatter_of_out_channel out in
  Utils.Printer.pp_file formatter (Ast.get ());
  Format.pp_print_flush formatter ();
  close_out out;

  (* output label data *)
  let data_filename = (Filename.chop_extension filename) ^ ".labels" in
  Options.feedback "write label data (to %s)" data_filename;
  let out = open_out data_filename in
  store_label_data out annotations;
  close_out out;
  Options.feedback "finished";
  Project.set_current base_project
  ;;

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
      Options.abort "`-main` parameter missing"
  | Dynamic.Unbound_value(s) -> Options.fatal "%s unbound" s
  | Dynamic.Incompatible_type(s) -> Options.fatal "%s incompatible" s
  | Failure s -> Options.fatal "unexpected failure: %s" s
  | e -> Options.fatal "unexpected exception: %s" (Printexc.to_string e)

let run () =
  if Options.ListAnnotators.get () then
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

