open Buffer
open Cil
open Cil_types

exception AlarmsSameLine



(* val get_alarms : unit -> stmt list *)
let get_alarms() =
  let ret = ref [] in

  let v = object
    inherit Visitor.frama_c_inplace
    method! vstmt_aux s =
      let _ = Utils.all_stmts := s :: !Utils.all_stmts in DoChildren
  end in
  let file = Ast.get () in
  let f = function
    | GFun(fd, _) -> ignore ( visitCilBlock (v :> Cil.cilVisitor) fd.sbody )
    | _ -> () in
  let _ = List.iter f file.globals in
  List.rev !ret

(*************************************************)


(*
module Make (ST : Instru.Type) = struct
  let run() = 
    Options.feedback "started";
    let _ = Globals.entry_point () in
    let alarms = get_alarms() in
      List.iter (fun x ->
		   Options.debug ~level:1 "alarm l.%i" (Utils.get_stmt_loc_int x)
		) alarms;
      let _verdicts = ST.process alarms in
	Options.feedback "finished";
end
*)
  
let pc_openFile filename = 
  try open_out filename
  with _ -> failwith ("could not open file " ^ filename)

let writeBufferInFile myBuffer filename = 
  let out = pc_openFile filename in
    output_buffer out myBuffer;
    close_out out

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
  let prj = File.create_project_from_visitor prj_name
    (fun prj -> new Visitor.frama_c_copy prj) in
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
(*
  (let alarmsBuffer = Buffer.create 1024 in
    Instru.prepareLabelsBuffer Instru.labelsList alarmsBuffer;
    writeBufferInFile alarmsBuffer ("labels.xml");
  )
*)

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
    | AlarmsSameLine ->
	Options.feedback "only 1 alarm per line for correct results"
    | Failure s -> Options.feedback "failure: %s" s
    | e -> Options.feedback "exception: %s" (Printexc.to_string e)
	
let run () =
  if not (Options.Annotators.is_empty ()) then
    let deps = [Ast.self] in
    let f, _self = State_builder.apply_once "GENLABELS" deps run in
      Kernel.LogicalOperators.on ();
      f ();
      Kernel.LogicalOperators.off ()

let () =
  Db.Main.extend run

