(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's Lannotate plug-in.                 *)
(*                                                                        *)
(*  Copyright (C) 2012-2022                                               *)
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
(*  for more details (enclosed in the file LICENSE)                       *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Ast_const

include Annotators.Register (struct

    let name = "FC"
    let help = "Function Coverage"

    (** A visitor that adds a label at the start of each function's body *)
    let visitor mk_label = object
      inherit Visitor.frama_c_inplace

      method! vfunc dec =
        if  Annotators.shouldInstrumentFun dec.svar then begin
          let label = mk_label (Exp_builder.one()) [] dec.svar.vdecl in
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

let compute_hl caller_callee =
  let disj =
    String.concat "+" (List.rev (List.map (fun i -> "l" ^ string_of_int i) (Hashtbl.find_all disjunctions caller_callee)))
  in
  Annotators.next_hl() ^ ") <" ^ disj ^ "|; ;>,"

let gen_hyperlabels_callcov = ref (fun () ->
    let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
    Options.feedback "write hyperlabel data (to %s)" data_filename;
    let out = open_out_gen [Open_creat; Open_append] 0o644 data_filename in
    output_string out (HL.fold (fun el str -> str ^ (compute_hl el) ^ "\n") !hyperlabels "");
    close_out out)

(** Call Coverage Visitor **)
class visitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  val mutable fname = ""
  val mutable floc = Cil_datatype.Location.unknown

  method private mk_call v =
    let newStmt = mk_label (Exp_builder.one()) [] floc in
    Hashtbl.add disjunctions (fname,v.vname) (Annotators.getCurrentLabelId());
    hyperlabels := (HL.add (fname,v.vname) !hyperlabels);
    newStmt

  method! vfunc dec =
    if Annotators.shouldInstrumentFun dec.svar then begin
      fname <- dec.svar.vname;
      floc <- dec.svar.vdecl;
      Cil.DoChildren
    end
    else
      Cil.SkipChildren

  method! vstmt_aux stmt =
    begin match stmt.skind with
      | Instr i when not (Utils.is_label i) ->
        begin match i with
          | Call (_,{eid = _;enode = Lval(Var v,_);eloc = _},_,_)
          | Local_init (_,ConsInit(v, _,_),_) ->
            let newStmt = self#mk_call v in
            stmt.skind <- Block (Cil.mkBlock [newStmt; Stmt_builder.mk stmt.skind]);
            Cil.SkipChildren
          | _ -> Cil.DoChildren
        end
      | _ -> Cil.DoChildren
    end

end

(**
   Call coverage annotator
*)
module CallCov = Annotators.Register (struct
    let name = "FCC"
    let help = "Function Call Coverage"
    let apply mk_label file =
      Visitor.visitFramacFileSameGlobals
        (new visitor mk_label :> Visitor.frama_c_visitor) file;
      !gen_hyperlabels_callcov ()
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
    let apply _ file =
      Visitor.visitFramacFileSameGlobals (new nopvisitor :> Visitor.frama_c_visitor) file
  end)
