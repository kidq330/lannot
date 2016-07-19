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

open Cil_types
open Annotators
open Ast_const

include Register (struct

    let name = "FC"
    let help = "Function Coverage"
    
    (** A visitor that adds a label at the start of each function's body *)
    let visitor mk_label = object
      inherit Visitor.frama_c_inplace

      method! vfunc dec =
        if shouldInstrument dec.svar then begin
          let label = mk_label (Cil.one Cil_datatype.Location.unknown) [] dec.svar.vdecl in
          dec.sbody.bstmts <- label :: dec.sbody.bstmts;
        end;
        Cil.SkipChildren
    end

    let apply f ast =
      Visitor.visitFramacFileSameGlobals (visitor f) ast

  end)


module StringString = struct
  type t = string * string
  let compare = compare
end
module HL = Set.Make(StringString)
let hyperlabels = ref HL.empty

let label_id = ref 0
let disjunctions = Hashtbl.create 100
 
let compute_hl caller_callee = "<" ^ (String.concat "+" (List.map (fun i -> "l" ^ string_of_int i) (Hashtbl.find_all disjunctions caller_callee))) ^ "|; ;>,"

let gen_hyperlabels_callcov = ref (fun () -> 
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let out = open_out data_filename in
  output_string out (HL.fold (fun el str -> (compute_hl el) ^ "\n" ^ str) !hyperlabels "");
  close_out out;
  Options.feedback "finished")


(** Call Coverage Visitor **)
class visitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  val mutable current_func = ""

  method! vfunc dec =
    current_func <- dec.svar.vname;
    Cil.DoChildren

  method! vstmt_aux stmt = match stmt.skind with
	| Instr i -> 
		(match i with 
     			| Call (_,func,_,_) -> (match func.enode with Lval (h,o) -> (match h with Var v ->
			   incr label_id; 
			   Hashtbl.add disjunctions (current_func,v.vname) !label_id;
			   hyperlabels := (HL.add (current_func,v.vname) !hyperlabels);
                           let oneExp = (Cil.integer Cil_datatype.Location.unknown 1) in
			   let idExp = (Cil.integer Cil_datatype.Location.unknown !label_id) in
			   let ccExp = (Cil.mkString Cil_datatype.Location.unknown "CallCov") in
			   let newStmt = (Utils.mk_call "pc_label" ([oneExp;idExp;ccExp])) in
			   Cil.ChangeTo (Stmt.block [ newStmt ; stmt]) | _ -> Cil.DoChildren ) | _ -> Cil.DoChildren) 
     			| _ -> Cil.DoChildren)
	| _ -> Cil.DoChildren
end


(**
   Call coverage annotator
*)
module CallCov = Annotators.Register (struct
    let name = "CallCov"
    let help = "Call Coverage"   
    let apply mk_label file = Visitor.visitFramacFileSameGlobals (new visitor mk_label :> Visitor.frama_c_visitor) file;
			      !gen_hyperlabels_callcov ()
  end)
