(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2020                                               *)
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
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Ast_const

let unk_loc = Cil_datatype.Location.unknown

type cm = Simple of int | Double of int

(** Sanity Check Mutation Visitor **)
class visitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  val mutable to_remove = []
  val mutable current_fdec = None
  val seen_if_macro = Stack.create ()
  val id_to_vinfos = Hashtbl.create 10
  val id = ref 0

  method private next () =
    incr id; !id

  method private get_vinfo id =
    let fdec = Extlib.the current_fdec in
    let vi = Cil.makeTempVar ~name:("lannot_mut_"^string_of_int (self#next())) fdec (TInt(IInt,[])) in
    if Hashtbl.mem id_to_vinfos id then
      Hashtbl.replace id_to_vinfos id (vi::(Hashtbl.find id_to_vinfos id))
    else
      Hashtbl.add id_to_vinfos id [vi];
    vi

  method private generate_exp e id =
    let vInfo = self#get_vinfo id in
    let loc = e.eloc in
    let lval = Cil.new_exp ~loc (Lval (Cil.var vInfo)) in
    let mut = Cil.mkBinOp ~loc LAnd lval (Exp.lnot e) in
    let not_mut = Cil.mkBinOp ~loc LAnd (Exp.lnot lval) e in
    let init = Utils.mk_call ~result:(Cil.var vInfo) "mutate" [] in
    init, Cil.mkBinOp ~loc LOr mut not_mut

  method private generate_if_exp e =
    match Stack.pop_opt seen_if_macro with
    | None -> [], e
    | Some (Simple id) ->
      let init, e = self#generate_exp e id in
      [init], e
    | Some (Double id) ->
      begin match e.enode with
        | BinOp (LAnd|LOr as op, e1, e2, _) ->
          let init, new_e1 = self#generate_exp e1 id in
          let init',new_e2 = self#generate_exp e2 id in
          [init;init'],Cil.mkBinOp e.eloc op new_e1 new_e2
        | _ -> assert false
      end

  method private generate_disj loc vinfos =
    let rec aux vinfos acc =
      match vinfos, acc with
      | [], Some acc -> acc
      | vInfo :: tl, None ->
        aux tl (Some(Cil.new_exp ~loc (Lval (Cil.var vInfo))))
      | vInfo :: tl, Some acc ->
        let new_lval = (Cil.new_exp ~loc (Lval (Cil.var vInfo))) in
        aux tl (Some(Cil.mkBinOp ~loc LOr new_lval acc))
      | _ -> assert false
    in
    aux vinfos None

  (* visit each function and annotate formals and return statement *)
  method! vfunc fdec =
    current_fdec <- Some fdec;
    Cil.DoChildrenPost( fun fdec ->
        to_remove <- [];
        current_fdec <- None;
        Stack.clear seen_if_macro;
        Hashtbl.clear id_to_vinfos;
        fdec
      )

  method! vblock _ =
    Cil.DoChildrenPost (fun b ->
        b.bstmts <- List.filter (fun stmt ->
            not (List.exists (fun sid ->
                sid = stmt.sid) to_remove)
          ) b.bstmts;
        b
      )

  (* Create bound labels for return statement  *)
  method! vstmt_aux stmt =
    match stmt.skind with
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, clean :: id :: [], loc)) when v.vname = "__LANNOTATE_SUCCESS" ->
      let clean = Integer.to_int (Extlib.the (Cil.isInteger clean)) in
      let id = Integer.to_int (Extlib.the (Cil.isInteger id)) in
      if Hashtbl.mem id_to_vinfos id then begin
        let vinfos = Hashtbl.find id_to_vinfos id in
        let label = mk_label (self#generate_disj loc vinfos) [] loc in
        if clean <> 0 then Hashtbl.remove id_to_vinfos id;
        Cil.ChangeTo label
      end
      else
        assert false
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, id :: [], _)) when v.vname = "__LANNOTATE_SIMPLE" ->
      to_remove <- stmt.sid :: to_remove;
      let id = Integer.to_int (Extlib.the (Cil.isInteger id)) in
      Stack.push (Simple(id)) seen_if_macro;
      Cil.SkipChildren
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, id :: [], _)) when v.vname = "__LANNOTATE_DOUBLE" ->
      to_remove <- stmt.sid :: to_remove;
      let id = Integer.to_int (Extlib.the (Cil.isInteger id)) in
      Stack.push (Double(id)) seen_if_macro;
      Cil.SkipChildren
    | If (exp,th,el,lo) ->
      let init_l, new_exp = self#generate_if_exp exp in
      let thenb = (Cil.visitCilBlock (self :> Cil.cilVisitor) th) in
      let elseb = (Cil.visitCilBlock (self :> Cil.cilVisitor) el) in
      if init_l != [] then
        stmt.skind <- Block (Block.mk (init_l @ [Stmt.mk (If (new_exp,thenb,elseb,lo))]))
      else
        stmt.skind <- If (new_exp,thenb,elseb,lo);
      Cil.SkipChildren
    | _ -> Cil.DoChildren

end

module SanityCheckMutation =   Annotators.Register (struct
    let name = "SCM"
    let help = "Sanity Check Mutation Coverage"
    let apply mk_label file =
      Visitor.visitFramacFileSameGlobals (new visitor mk_label :> Visitor.frama_c_visitor) file
  end)