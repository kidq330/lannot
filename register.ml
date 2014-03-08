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
  
let pc_openFile filename = 
  try open_out filename
  with _ -> failwith ("could not open file " ^ filename)

let writeBufferInFile myBuffer filename = 
  let out = pc_openFile filename in
    output_buffer out myBuffer;
    close_out out


(* ENTRY POINT *)
let run () =
  Instru.multiCondOption := Options.MultiCond.get();
  Instru.aorOption := Options.AOR.get ();
  Instru.rorOption := Options.ROR.get ();
  Instru.corOption := Options.COR.get ();
  Instru.absOption := Options.ABS.get ();
  Instru.partitionOption := Options.PARTITION.get ();
  Instru.simpleOption := Options.SimpleCond.get ();
  try
    if !Instru.aorOption = true or !Instru.rorOption = true 
      or !Instru.corOption = true or !Instru.absOption = true 
      or !Instru.partitionOption = true then
	begin
	  let module M = Make (Instru.WM ) in
            M.run()
	end;
    if !Instru.multiCondOption = true then
      begin
        let module M = Make (Instru.MCC ) in
          M.run()
      end;
    if !Instru.simpleOption = true then
      begin
        let module M = Make (Instru.CC ) in
          M.run()
      end;    
    (let alarmsBuffer = Buffer.create 1024 in
       Instru.prepareLabelsBuffer Instru.labelsList alarmsBuffer;
       writeBufferInFile alarmsBuffer ("labels.xml");
    )
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
  if Options.Enabled.get () then
    let deps = [Ast.self; Options.Enabled.self] in
    let f, _self = State_builder.apply_once "GENLABELS" deps run in
      Kernel.LogicalOperators.on ();
      f ();
      Kernel.LogicalOperators.off ()

let () =
  Db.Main.extend run

