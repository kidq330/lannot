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
open Ast_const

let cantor_pairing n m = (((n+m)*(n+m+1))/2)+m
let idList = ref []

let mkSet fid loopId =
  let id = cantor_pairing fid loopId in
  idList := id :: !idList;
  let idExp = Exp.integer id in
  let oneExp = Exp.one () in
  let twoExp = Exp.one () in
  let twoExptwo = Exp.integer 2 in
  let zeroExp = Exp.zero() in
  let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int id))) in
  Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])

let mkUse fid loopId =
  let id = cantor_pairing fid loopId in
  idList := id :: !idList;
  let idExp = Exp.integer id in
  let oneExp = Exp.one () in
  let twoExp = Exp.integer 2 in
  let twoExptwo = Exp.integer 2 in
  let zeroExp = Exp.zero() in
  let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int id))) in
  Utils.mk_call "pc_label_sequence" ([oneExp;idExp;twoExp;twoExptwo;ccExp;zeroExp])

let mkloopcond fid loopId =
  let id = cantor_pairing fid loopId in
  let ccExp = (Cil.mkString Cil_datatype.Location.unknown ((string_of_int id))) in
  let cond = (Utils.mk_call "pc_label_sequence_condition" ([Exp.zero();ccExp])) in
  cond

let visitor = object(_)
      inherit Visitor.frama_c_inplace

      val mutable first = false
      val mutable fid = 0
      val mutable loopId = 0
      method! vfunc dec =
        if Annotators.shouldInstrument dec.svar then begin
          fid <- dec.svar.vid;
          Cil.DoChildren
        end
        else
          Cil.SkipChildren

      method! vstmt_aux stmt =
        match stmt.skind with
        | If (e,th,el,l) ->
          if first then begin
            let cond = mkloopcond fid loopId in
            first <- false;
            stmt.skind <- (If (e,{th with bstmts = cond::th.bstmts},el,l));
            Cil.ChangeTo stmt
          end
          else
            Cil.DoChildren
        | Loop _ ->
          first <- true;
          loopId <- stmt.sid;
          let set = mkSet fid loopId in
          let use = mkUse fid loopId in
          let local_loopid = loopId in
          Cil.DoChildrenPost (fun stmt ->
              if not first then
                Stmt.block [set;stmt;use]
              else begin
                match stmt.skind with
                | Loop (ca,b,l,s1,s2) ->
                  first <- false;
                  let cond = mkloopcond fid local_loopid in
                  let nstmt = {stmt with skind = Loop (ca,{b with bstmts = cond::b.bstmts},l,s1,s2)} in
                  Stmt.block [set;nstmt;use]
                | _ -> assert false
              end
            )
         | _ ->
           Cil.DoChildren
    end

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = String.concat "\n" (List.map (fun i -> "<s" ^ string_of_int i ^"|; ;>,") (List.rev !idList))in
  let out = open_out_gen [Open_creat; Open_append] 0o640 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of labels = %d" ((List.length !idList)*2);
  Options.feedback "finished"

include Annotators.Register (struct

    let name = "LOOP"
    let help = "Loop Coverage"

    (** A visitor that adds a label at the start of each loop *)

    let apply _ ast =
      Visitor.visitFramacFileSameGlobals visitor ast;
      gen_hyperlabels ()
  end)
