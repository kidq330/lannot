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


(*class mutator mut prj = object(_self)
    inherit Visitor.generic_frama_c_visitor(Cil.copy_visit prj)

val mutable visit = 1 

method! vfunc f = SkipChildren

  method! vstmt_aux stmt = let genmuts s =
      if visit == 1 then 
      match s.skind with
	| Instr(Set(_l, e, loc)) ->
	    s.skind <- Instr(Set(_l, mut, loc)); visit <- visit + 1;s
		
	| If(e, _l, _ll, loc) ->
	    s.skind <- If(mut, _l, _ll, loc); visit <- visit + 1;s
	      
	| Return(e, loc) ->
            begin match e with 
	      | Some exp ->
		  s.skind <- Return(e, loc); visit <- visit + 1;s
	      | _ -> s 
	    end
	      
	| Switch(e, _l,_ll, loc) ->
	    s.skind <- Switch(mut, _l, _ll, loc); visit <- visit + 1;s
	      
        | _ -> s
    else 
       s
    in  
      ChangeDoChildrenPost (stmt, genmuts)

end*)

class mutate origexp mut = object(_self)
    inherit Visitor.frama_c_inplace

 (* method! vexpr exp = if exp.eid = origexp.eid then begin s.skind <- Instr(Set(_l, mut, loc)); end else DoChildren *)

  method! vstmt_aux stmt = let genmuts s =
      match s.skind with
	| Instr(Set(_l, e, loc)) ->
	     if e.eid = origexp.eid then begin s.skind <- Instr(Set(_l, mut, loc));s end else s

	| If(e, _l, _ll, loc) ->
	    if e.eid = origexp.eid then begin s.skind <- If(mut, _l, _ll, loc);s  end else s

	| Return(e, loc) ->
            begin match e with 
	      | Some exp -> (* todo consider return mutation, now is ignored *)
		 if exp.eid = origexp.eid then begin s.skind <- Return(e, loc);s end else s

	      | _ -> s 
	    end
	      
	| Switch(e, _l,_ll, loc) ->
	    if e.eid = origexp.eid then begin s.skind <- Switch(mut, _l, _ll, loc);s end else s
	      
        | _ -> s
     in  
      ChangeDoChildrenPost (stmt, genmuts)

end

class strongmutationLabelsVisitor = object(_self)
    inherit Visitor.frama_c_inplace

    method! vfunc f =
      if Annotators.shouldInstrument f.svar then DoChildren else SkipChildren

  method! vstmt_aux stmt =      	    
    let traitStmt s loc = 
      if Instru.Labels.mem s.sid !Instru.mylabels then (*let mm = Instru.Labels.find s.sid !Instru.mylabels in Options.feedback " stmt:  %a " Printer.pp_stmt mm;*) 
let empty l = match l with | [] -> true | Case(_e,_loc) :: tl -> false | _ -> true in if empty s.labels then Instru.Labels.find s.sid !Instru.mylabels else begin let stmt = Instru.Labels.find s.sid !Instru.mylabels in stmt.labels <- s.labels ; s.labels <- [] ; stmt end
else s
	(*Instru.Labels.find s.sid label_map*)
    in
    let genLabels s =
      match s.skind with
	| Instr(Set(_l, e, loc)) ->
	    traitStmt s loc

	| Instr(Call(_l, e, args, loc)) ->
	    traitStmt s loc

	| If(e, _l, _ll, loc) ->
	    traitStmt s loc;

        | Continue(loc) -> traitStmt s loc

        | Break(loc) -> traitStmt s loc

	| Return(e, loc) ->
	    begin match e with 
	      | Some exp ->
		  traitStmt s loc
	      | _ -> s 
	    end
	    	      
	| Switch(e, _l,_ll, loc) ->
	    traitStmt s loc
	      
      | _ -> s
    in  
      ChangeDoChildrenPost (stmt, genLabels)
end

(*  let compute label_map ast =
    Visitor.visitFramacFileSameGlobals
      (new strongmutationLabelsVisitor label_map :> Visitor.frama_c_inplace)
      ast
end*)



(*

class mutate origexp mut = object(_self)
    inherit Visitor.frama_c_inplace

val mutable visit = 1 

method! vfunc f = SkipChildren

  method! vstmt_aux stmt = let genmuts s =
      if visit == 1 then 
      match s.skind with
	| Instr(Set(_l, e, loc)) ->
	    Options.feedback "I %d" (fst  e.eloc).Lexing.pos_lnum;s.skind <- Instr(Set(_l, mut, loc)); visit <- visit + 1;s

	| If(e, _l, _ll, loc) ->
	    s.skind <- If(mut, _l, _ll, loc); visit <- visit + 1;s
	      
	| Return(e, loc) ->
            begin match e with 
	      | Some exp ->
		  s.skind <- Return(e, loc); visit <- visit + 1;s
	      | _ -> s 
	    end
	      
	| Switch(e, _l,_ll, loc) ->
	    s.skind <- Switch(mut, _l, _ll, loc); visit <- visit + 1;s
	      
        | _ -> s
    else 
       s
    in  
      ChangeDoChildrenPost (stmt, genmuts)

end

*)




let store_label_data out annotations =
  (* TODO do that in its own module, ultimately shared with the other LTest-tools *)
  (* TODO (later) do something better than csv *)
  let formatter = Format.formatter_of_out_channel out in
  Format.fprintf formatter "# id, status, tags, file:Line, origin, mut, @.";
  let print_one (id, tags, _stmt, origin_loc, origexp, mut, origstmt, mutstmt) =
    let origin_file = (fst  origin_loc).Lexing.pos_fname in
    let origin_line = (fst  origin_loc).Lexing.pos_lnum in
    Format.fprintf formatter "%d, unknown, %s, %s:%d@." id tags origin_file origin_line (*Printer.pp_exp origexp Printer.pp_exp mut*)  
  in
  List.iter print_one annotations;
  Format.pp_print_flush formatter ()

let make_mutants prje annotations =
  let make_mut (id, _tags, stmt, _origin_loc, origexp, mut, origstmt, mutstmt) =
    (* let origstatement = Cil.mkStmt stmt.skind; in *)
(*if id = 8 then begin *)
(*    let filename = (Project.get_name prje) ^ string_of_int id ^ ".c" in *)
let origskind = stmt.skind in
    let filename = (string_of_int id) ^ "/" ^ (Project.get_name prje) ^ ".c" in
    Utils.mkdir (string_of_int id);
if origstmt.sid = stmt.sid then begin
   stmt.skind <- mutstmt.skind;
end 
else begin
    Visitor.visitFramacStmt (new mutate origexp mut :> Visitor.frama_c_inplace) stmt;()(*Options.feedback "ID: %d, MutExp: %a, OrigExp: %a stmt:  %a " id Printer.pp_exp mut Printer.pp_exp origexp Printer.pp_stmt stmt; *)
end;
    let out = open_out filename in
    let formatter = Format.formatter_of_out_channel out in
    File.pretty_ast ~prj:prje ~fmt:formatter ();
    Format.pp_print_flush formatter ();
    close_out out;
(*end *)
(*    stmt.skind <- skind; *)
(*    Project.set_current prje;*)
(*    stmt.skind <- origstatement.skind; () *)
if origstmt.sid = stmt.sid then begin
   stmt.skind <- origskind 
end else begin
    Visitor.visitFramacStmt (new mutate mut origexp :> Visitor.frama_c_inplace) stmt;()(*Options.feedback "ID: %d, MutExp: %a, OrigExp: %a stmt:  %a" id Printer.pp_exp mut Printer.pp_exp origexp Printer.pp_stmt stmt;  *)
end
  in
  List.iter make_mut annotations;;

(*
let strongMutation ann_names prj annotations =
  (* output the mutants *)
  if List.hd ann_names = "SM" then begin
  Options.feedback "write mutant files";
  make_mutants prj annotations
end
*)

let chooseAnotator prj ann_names ast =
 if List.hd ann_names = "SM" then begin
Options.feedback "write original file";
    let filename = (Project.get_name prj) ^ "-orig.c" in
    let out2 = open_out ( (Project.get_name prj) ^ ".strong" ) in
    let out = open_out filename in
    let formatter = Format.formatter_of_out_channel out in
    File.pretty_ast ~prj:prj ~fmt:formatter ();
    Format.pp_print_flush formatter ();
    close_out out;
    Utils.formatter := (Format.formatter_of_out_channel (out2));
    let annotations = Annotators.strongmutation ann_names ast in
    Options.feedback "write mutant files";
    close_out out2;
 (*   make_mutants prj annotations; (* make_mutants prints the mutants to files *) *)
    Visitor.visitFramacFileSameGlobals (new strongmutationLabelsVisitor :> Visitor.frama_c_inplace) ast; 
    annotations
  end
  else Annotators.annotate ann_names ast


let annotate ann_names =
  Options.feedback "started";
  let prj_name = Config.input_file () in
  let prj = File.create_project_from_visitor prj_name
    (fun prj -> new Visitor.frama_c_copy prj) in
  (* TODO use an options to set the output filename *)
  let filename = (Project.get_name prj) ^ "_labels.c" in
  let data_filename = (Project.get_name prj) ^ "_labels.labels" in
  Project.set_current prj;
  let annotations = chooseAnotator prj ann_names (Ast.get ()) in
  (*let annotations = Annotators.annotate ann_names (Ast.get ()) in
  strongMutation ann_names prj annotations; *)

  (* output modified c file *)
  Options.feedback "write Labels to the C file";
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
    if mutname = "AOR" then Instru.aorOption := false
    else if mutname = "COR" then Instru.corOption := false
    else if mutname = "ABS" then Instru.absOption := false
    else if mutname = "ROR" then Instru.rorOption := false
    else if mutname = "BTW" then Instru.btworOption := false
    else if mutname = "SFT" then Instru.shiftOption := false
    else if mutname = "PM" then Instru.pmOption := false
    else if mutname = "CRC" then Instru.crcOption := false
    else if mutname = "VRRL" then Instru.vrrLOption := false
    else if mutname = "VRRG" then Instru.vrrGOption := true 
    else if mutname = "UOI" then Instru.uoiOption := false
    else if mutname = "SDL" then Instru.sdlOption := false
in
  Options.Mutators.iter f



(* ENTRY POINT *)
let run () =
  try
  (*  setupMutatorOptions (); *)
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

