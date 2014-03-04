open Lexing
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


module Make (ST : Slicing.Type) = struct
  let run() = 
    Options.Self.feedback "started";
    let _ = Globals.entry_point () in
    let alarms = get_alarms() in
      List.iter (fun x ->
		   Options.Self.debug ~level:1 "alarm l.%i" (Utils.get_stmt_loc_int x)
		) alarms;
      let _verdicts = ST.process alarms in
	Options.Self.feedback "finished";
end
  
let pc_openFile filename = 
  try open_out filename
  with _ -> failwith ("could not open file " ^ filename)

let writeBufferInFile myBuffer filename = 
  let out = pc_openFile filename in
    output_buffer out myBuffer;
    close_out out


(* ENTRY POINT *)
let run () =
  let multiCondOption = Options.MultiCond.get() in
  let aorOption = Options.AOR.get () in
  let rorOPtion = Options.ROR.get () in
  let corOPtion = Options.COR.get () in
  let absOption = Options.ABS.get () in
  let partitionOption = Options.PARTITION.get () in
  let simpleOption = Options.SimpleCond.get () in
  try
    if multiCondOption then
      begin
        let module M = Make (Slicing.Multi ) in
          M.run()
      end;
    if aorOption then
      begin
	let module M = Make (Slicing.Aor) in
	  M.run()
      end;
    if rorOPtion then
      begin
	let module M = Make (Slicing.Ror) in
	  M.run()
      end;
    if corOPtion then
      begin
	let module M = Make (Slicing.Cor) in
	  M.run()
      end;
    if absOption then
      begin
	let module M = Make (Slicing.Abs) in
	  M.run()
      end;
    if partitionOption then
      begin
	let module M = Make (Slicing.Partition) in
	  M.run()
      end;
    if simpleOption then
      begin
        let module M = Make (Slicing.None ) in
          M.run()
      end;
    (let alarmsBuffer = Buffer.create 1024 in
       Instru.prepareLabelsBuffer Instru.labelsList alarmsBuffer;
       writeBufferInFile alarmsBuffer ("labels.xml");
    )
  with
  | Globals.No_such_entry_point _ ->
      Options.Self.feedback "`-main` parameter missing"
  | Dynamic.Unbound_value(s) -> Options.Self.feedback "%s unbound" s
  | Dynamic.Incompatible_type(s) -> Options.Self.feedback "%s incompatible" s
  | Config.NoInputFile -> Options.Self.feedback "no input file"
  | AlarmsSameLine ->
      Options.Self.feedback "only 1 alarm per line for correct results"
  | Failure s -> Options.Self.feedback "failure: %s" s
  | e -> Options.Self.feedback "exception: %s" (Printexc.to_string e)

let run () =
  if Options.Enabled.get () then
    let deps = [Ast.self; Options.Enabled.self] in  
    let f, _self = State_builder.apply_once "GENLABELS" deps run in
      f ()
  
let () =
  Db.Main.extend run

