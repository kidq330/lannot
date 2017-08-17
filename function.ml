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
class visitor = object(_)
  inherit Visitor.frama_c_inplace

  val mutable current_func = ""

  method! vfunc dec =
    current_func <- dec.svar.vname;
    Cil.DoChildren

  method! vstmt_aux stmt = match stmt.skind with
	| Instr i -> 
		(match i with 
     			| Call (_,func,_,_) -> (match func.enode with Lval (h,_) -> (match h with Var v ->
			   incr label_id; 
			   Hashtbl.add disjunctions (current_func,v.vname) !label_id;
			   hyperlabels := (HL.add (current_func,v.vname) !hyperlabels);
                           let oneExp = (Cil.integer Cil_datatype.Location.unknown 1) in
			   let idExp = (Cil.integer Cil_datatype.Location.unknown !label_id) in
			   let ccExp = (Cil.mkString Cil_datatype.Location.unknown "FCC") in
			   let newStmt = (Utils.mk_call "pc_label" ([oneExp;idExp;ccExp])) in
			   Cil.ChangeTo (Stmt.block [ newStmt ; stmt]) | _ -> Cil.DoChildren ) | _ -> Cil.DoChildren) 
     			| _ -> Cil.DoChildren)
	| _ -> Cil.DoChildren
end


(**
   Call coverage annotator
*)
module CallCov = Annotators.Register (struct
    let name = "FCC"
    let help = "Function Call Coverage"   
    let apply mk_label file = ignore mk_label; (* Avoid warning about mk_label unused *)
			      Visitor.visitFramacFileSameGlobals (new visitor :> Visitor.frama_c_visitor) file;
			      !gen_hyperlabels_callcov ()
  end)

let cplTyToNbr = Hashtbl.create 100
let id_gen = ref 0
let list_convert = ref []
let oc = ref None
let get_oc () = match !oc with | Some f -> f | None -> let f = (open_out "output.txt") in oc := Some f; f  

(** Remove Cast Visitor **)
class remcastvisitor = object(_)
  inherit Visitor.frama_c_inplace
  
  val mutable current_func = Cil.emptyFunction ""

  method! vfunc dec =
    current_func <- dec;
    Cil.DoChildren
    
    
  
  method! vexpr e = match e.enode
		    with | CastE (_,_) -> let f res = match res.enode with | CastE (tt1,ce) ->
		    			    let tt2 = Cil.typeOf ce in 
		    			    (Printer.pp_typ (Format.str_formatter) tt2) ;
		    			    let t2 = (Format.flush_str_formatter ()) in	    			    
		    			    (Printer.pp_typ (Format.str_formatter) tt1) ;
		    			    let t1 = (Format.flush_str_formatter ()) in
		    			    let nb_fun_repl = if Hashtbl.mem cplTyToNbr (t2,t1) then (
		    			    	Hashtbl.find cplTyToNbr (t2,t1)
		    			    ) else (
		    			    	incr id_gen;
		    			    	let nb = !id_gen in
		    			    	Hashtbl.add cplTyToNbr (t2,t1) !id_gen;
		    			    	Printf.fprintf (get_oc ()) "%s\n" ("/*@assigns \\nothing;*/ "^t1^" convert_"^(string_of_int nb)^"("^t2^" input);");
		    			    	nb
		    			    ) in
		    			    let convert_fun_name = "convert_"^(string_of_int nb_fun_repl) in
		    			    let convert_result_info = (Cil.makeTempVar current_func tt1) in
		    			    let convert_result = (Cil.var convert_result_info) in
		    			    let convert_call = (Utils.mk_call ~result:convert_result convert_fun_name ([ce])) in
		    			    list_convert := convert_call::!list_convert;
		    			    (Cil.evar convert_result_info) 
		    			    | _ -> failwith "Unexpected"
		    			    in (Cil.DoChildrenPost f) 		    		
		         | _ -> Cil.DoChildren
   
   method! vstmt_aux _ = let f res =
   			    if not ((List.length !list_convert)=0) then (	
   			    	let listr = List.rev (res::!list_convert) in
   			    	list_convert := [];
   			    	(Stmt.block listr)  			    
   			    ) else res in (Cil.DoChildrenPost f)
   			    
  
end


(**
   Remove cast annotator
*)
module CastRem = Annotators.Register (struct
    let name = "CastRem"
    let help = "Process file and replace cast by function calls"   
    let apply mk_label file = ignore mk_label; (* Avoid warning about mk_label unused *)
			      Visitor.visitFramacFileSameGlobals (new remcastvisitor :> Visitor.frama_c_visitor) file
  end)



(** Nop Visitor **)
class nopvisitor = object(_)
  inherit Visitor.frama_c_inplace
end


(**
   Call coverage annotator
*)
module Empty = Annotators.Register (struct
    let name = "Empty"
    let help = "Process file but add no label"   
    let apply mk_label file = ignore mk_label; (* Avoid warning about mk_label unused *)
			      Visitor.visitFramacFileSameGlobals (new nopvisitor :> Visitor.frama_c_visitor) file
  end)
