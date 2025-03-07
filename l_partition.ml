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

(**
   Partitions the domain of some l-value (as a function) given its type

   The l-value is specified through a function that must returns a fresh copy
   each time (with unique expression ids).

   [depth] is the maximal depth to go into the l-value and [width] is the maximal
   width for array and structures explorations.
*)
let rec partition_lval ~depth ~width ~(emit: exp -> unit) typ lval =
  Options.debug "partitioning l-value @[%a@] : @[%a@] (max_depth=%d)" Printer.pp_lval (lval ()) Printer.pp_typ typ depth;
  if depth >= 0 then
    match typ with
    | TVoid _ | TFun _ ->
      (* cannot do anything at this depth *)
      ()
    (* "Expressible" type *)
    | TInt _ | TFloat _ | TPtr _ | TEnum _ ->
      (* simple lval, do not use for struct or union *)
      let exp () = Exp_builder.lval (lval ()) in
      partition_exp ~depth ~width ~emit typ exp

    (* Named type *)
    | TNamed (typ,_) ->
      (* Compound type maybe named, so unwrap it here *)
      partition_lval ~depth ~width ~emit typ.ttype lval

    (* Structure or union (does not make much sense on union) *)
    | TComp (comp, _) ->
      let i = ref (-1) in
      let onfield field =
        incr i;
        if !i < width then
          let lval' () = Cil.addOffsetLval (Field (field, NoOffset)) (lval ()) in
          partition_lval ~depth:(depth-1) ~width ~emit field.ftype lval'
      in
      begin match comp.cfields with
        | Some cfields ->  List.iter onfield cfields
        | None -> ()
      end

    (* Array length provided *)
    | TArray (typ, Some length, _) ->
      begin match Cil.isInteger length with
        | Some l ->
          (* Constant length *)
          (* let's generate labels for each element (in the limit of [width] elements) *)
          let l = if Integer.lt l (Integer.of_int width) then Integer.to_int_exn l else width in
          for i = 0 to l-1 do
            let lval' () = Cil.addOffsetLval (Index (Exp_builder.integer i, NoOffset)) (lval ()) in
            partition_lval ~depth:(depth-1) ~width ~emit typ lval'
          done
        | _ ->
          (* Symbolic length *)
          emit (Exp_builder.binop Eq (Cil.copy_exp length) (Exp_builder.zero ()));
          for i = 0 to width-1 do
            let emit' cond = emit (Exp_builder.binop LAnd (Exp_builder.binop Gt (Cil.copy_exp length)
                                                             (Exp_builder.integer i)) cond) in
            let lval' () = Cil.addOffsetLval (Index (Exp_builder.integer i, NoOffset))
                (lval ()) in
            partition_lval ~depth:(depth-1) ~width ~emit:emit' typ lval'
          done
      end

    | TArray (typ, None, _) ->
      (* unknown size: let's check the first element *)
      let lval' () = Cil.addOffsetLval (Index (Exp_builder.zero (), NoOffset)) (lval ()) in
      partition_lval ~depth:(depth-1) ~width ~emit typ lval'

    | TBuiltin_va_list _ ->
      failwith "Builtin va list not supported by partition_lval"

(**
   Partitions the domain of some expression (as a function) given its type.

   See {!partition_lval} for details.
*)
and partition_exp ~depth ~width ~(emit : exp -> unit) typ exp =
  Options.debug "partitioning expression @[%a@] : @[%a@] (max_depth=%d)" Printer.pp_exp (exp ()) Printer.pp_typ typ depth;
  if depth >= 0 then
    match typ with

    (* Unsigned integers *)
    | TInt ((IUChar|IUShort|IUInt|IULong|IULongLong|IBool),_) ->
      List.iter emit [
        Exp_builder.binop Eq (exp ()) (Exp_builder.zero ());
        Exp_builder.binop Ne (exp ()) (Exp_builder.zero ())
      ];

      (* Signed integer and floating point numbers *)
    | TInt _ | TFloat _ ->
      List.iter emit [
        Exp_builder.binop Eq (exp ()) (Exp_builder.zero ());
        Exp_builder.binop Gt (exp ()) (Exp_builder.zero ());
        Exp_builder.binop Lt (exp ()) (Exp_builder.zero ())
      ]

    (* Pointers *)
    | TPtr (typ, _) ->
      emit (Exp_builder.binop Eq (exp ()) (Exp_builder.zero ()));
      emit (Exp_builder.binop Ne (exp ()) (Exp_builder.zero ()));
      let emit' cond = emit (Exp_builder.binop LAnd (Exp_builder.binop Ne (exp ()) (Exp_builder.zero ())) cond) in
      let lval' () = Cil.mkMem ~addr:(exp ()) ~off:NoOffset in
      partition_lval ~depth:(depth-1) ~width ~emit:emit' typ lval'

    (* Named type *)
    | TNamed (ti,_) ->
      partition_exp ~depth ~width ~emit ti.ttype exp

    (* Enumeration *)
    | TEnum (enum,_) ->
      let onitem item =
        emit (Exp_builder.binop Eq (exp ()) (Cil.copy_exp item.eival));
      in
      List.iter onitem enum.eitems

    | TVoid _ | TFun _ | TComp _ | TArray _ ->
      assert false

    | TBuiltin_va_list _ ->
      failwith "Builtin va list not supported by partition_exp"


class inputDomainVisitor max_depth max_width all_funs globals mk_label = object (self)
  inherit Visitor.frama_c_inplace

  method gformals vars loc =
    let acc = ref [] in
    let emit exp =
      if Cil.isIntegerConstant exp then
        () (* ignore trivially true or false exp *)
      else
        acc := mk_label exp [] loc :: !acc
    in
    let gen_for_var var =
      partition_lval
        ~depth:max_depth ~width:max_width ~emit var.vtype (fun ()-> Cil.var var)
    in
    List.iter gen_for_var vars;
    List.rev !acc

  method! vfunc f =
    if Annotators.shouldInstrumentFun f.svar &&
       (all_funs || f.svar.vname = Kernel.MainFunction.get ()) then begin
      let lbls = self#gformals (globals @ f.sformals) f.svar.vdecl in
      f.sbody.bstmts <- lbls @ f.sbody.bstmts
    end;
    Cil.SkipChildren
end

module Partition = Annotators.Register (struct

    let name = "IDP"
    let help = "Input Domain Partition"

    let apply mk_label ast =
      let max_depth = Options.MaxDepth.get () in
      let max_width = Options.MaxWidth.get () in
      let all_funs = Options.AllFuns.get () in
      let globals_as_input = Options.GlobalsAsInput.get () in
      Options.debug "input domain partition (max depth %d, max width %d, all funs? %b, globals as input? %b)" max_depth max_width all_funs globals_as_input;
      let globals = if globals_as_input then Utils.extract_global_vars ast else [] in
      Visitor.visitFramacFileSameGlobals
        (new inputDomainVisitor max_depth max_width all_funs globals mk_label :> Visitor.frama_c_inplace)
        ast

  end)
