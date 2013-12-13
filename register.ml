open Lexing
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
    (* Utils.mkdir (Config.result_dir());*)
    let alarms = get_alarms() in

	List.iter (fun x ->
	  Options.Self.debug ~level:1 "alarm l.%i" (Utils.get_stmt_loc_int x)
	) alarms;


	(******************************************)
	(* SLICING & DYNAMIC ANALYSIS             *)
	(******************************************)

	let _verdicts = ST.process alarms in

    Options.Self.feedback "finished";
end


(* ENTRY POINT *)
let run() =
  try
    if Options.MultiCond.get() then
      begin
	let module M = Make (Slicing.Multi ) in
	  M.run()
      end
    else
      begin
	let module M = Make (Slicing.None ) in
	  M.run()
      end
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

let run =
  let deps = [Ast.self; Options.Enabled.self] in
  Cil.set_useLogicalOperators true;
  let f, _self = State_builder.apply_once "GENLABELS" deps run in
  f


let() = Db.Main.extend run

