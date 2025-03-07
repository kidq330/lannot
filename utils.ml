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

let print_std_includes fmt globs =
  if not (Kernel.PrintLibc.get ()) then begin
    let extract_file acc = function
      | AStr s -> Datatype.String.Set.add s acc
      | _ -> Kernel.warning "Unexpected attribute parameter for fc_stdlib"; acc
    in
    let add_file acc g =
      let attrs = Cil_datatype.Global.attr g in
      List.fold_left extract_file acc (Cil.findAttribute "fc_stdlib" attrs)
    in
    let includes = List.fold_left add_file Datatype.String.Set.empty globs in
    let print_one_include s = Format.fprintf fmt "#include \"%s\"@." s in
    Datatype.String.Set.iter print_one_include includes
  end

module Printer = Printer_builder.Make (struct
    class printer () = object (self)
      inherit Printer.extensible_printer ()
      method! file fmt file =
        Format.fprintf fmt "@[/* Generated by Frama-C LTest */\n\n";
        print_std_includes fmt file.globals;
        Format.fprintf fmt "\n#ifndef pc_label\n#define pc_label(...) do{}while(0)\n\
                            #endif@\n\
                            #ifndef pc_label_bindings\n#define pc_label_bindings(...) do{}while(0)\n\
                            #endif@\n\n";
        if Options.Annotators.mem "RCC" then begin
          Format.fprintf fmt "\
          #define MAX_MUTATION %d\n\
          unsigned int cpt_mutation = 0;\n\n\
          /*%@ assigns cpt_mutation, \\result;\n\
          \    behavior can_mutate:\n\
          \        assumes cpt_mutation < MAX_MUTATION;\n\
          \        ensures  \\result <==> cpt_mutation == \\at(cpt_mutation, Pre) + 1;\n\
          \        ensures !\\result <==> cpt_mutation == \\at(cpt_mutation, Pre);\n\n\
          \    behavior cant_mutate:\n\
          \        assumes cpt_mutation >= MAX_MUTATION;\n\
          \        ensures !\\result;\n\
          \        ensures cpt_mutation == \\at(cpt_mutation, Pre);\n\n\
          \    complete behaviors;\n\
          \    disjoint behaviors;\n\
          */\n\
          int mutated(void);\n" (Options.MaxMutation.get ());
        end;
        Cil.iterGlobals file (fun g -> self#global fmt g);
        Format.fprintf fmt "@]@."
    end
  end)

(** Extracts global variables from an AST *)
let extract_global_vars file =
  let module S = Cil_datatype.Varinfo.Set in
  let f acc global =
    match global with
    | GVar (vi,_,_) -> if Cil.isFunctionType vi.vtype then acc else S.add vi acc
    | GVarDecl (vi,_) -> if Cil.isFunctionType vi.vtype then acc else S.add vi acc
    | _ -> acc
  in
  let globals = Cil.foldGlobals file f S.empty in
  S.elements globals

let print_file_path origine_loc = (Filepath.Normalized.to_pretty_string ((fst origine_loc).Filepath.pos_path))

let mk_call ?(loc=Cil_datatype.Location.unknown) ?result fvinfo args =
  let vinfo_exp = Exp_builder.var ~loc (Option.get fvinfo) in
  Stmt_builder.mk (Instr (Call (result, vinfo_exp, args, loc)))

(** Indicates whether an instruction is a label. *)
let is_label instr =
  match instr with
  | Call (_, {enode=Lval (Var {vname=name}, NoOffset)}, _, _) ->
    let regexp = Str.regexp_string "pc_label" in
    Str.string_match regexp name 0
  | _ -> false

(**
   Indicates whether an expression is boolean in itself.
   Used to detect boolean expression outside conditional statement
*)
let is_boolean e =
  Options.debug ~level:3 "is boolean? @[%a@]@." Cil_printer.pp_exp e;
  match e.enode with
  (* C99 _Bool type *)
  | BinOp (_, _, _, TInt (IBool, _))
  | UnOp (_, _, TInt (IBool, _))

  (* Use of logical operators or relational operators *)
  | BinOp ((LAnd|LOr|Lt|Gt|Le|Ge|Eq|Ne), _, _, _)
  | UnOp (LNot, _, _) -> true
  | _ -> false

(**
   Get atomic conditions from a boolean expression.
*)
let atomic_conditions =
  let rec aux acc exp =
    match exp.enode with
    | BinOp ((Ne|Eq), a, b, _) ->
      (* Cil adds !=0 when && or || are present in normal expression (don't do that for !)*)
      if Cil.isZero b && is_boolean a then
        aux acc a
      else if Cil.isZero a && is_boolean b then
        aux acc b
      else
        exp :: acc
    | BinOp ((LAnd | LOr), e1, e2, _) ->
      aux (aux acc e1) e2
    | UnOp (LNot, e, _) ->
      aux acc e
    | _ ->
      exp :: acc
  in
  fun exp -> List.rev (aux [] exp)

let rev_combine : int -> 'a list -> 'a list list =
  let rec comb_aux rev_startswith acc n l =
    match n, l with
    | 0, _ -> (List.rev rev_startswith) :: acc
    | _, [] -> acc
    | _, head :: tail ->
      let acc = comb_aux (head :: rev_startswith) acc (n-1) tail in
      comb_aux rev_startswith acc n tail
  in
  fun n l -> comb_aux [] [] n l (*Needed to keep polymorphism, wtf!?*)

(** [combine n l] computes the combinations of [n] elements from the list [l].

    Returns the combination in the order of the list [l].
    For instance, [combine 2 [1;2;3]] returns [[1;2];[1;3];[2;3]].
*)
let combine n (l : 'a list) : 'a list list =
  List.rev (rev_combine n l)

(**
   [sign_combine pos neg l] computes all sign combinations of a list of elements [l], given two sign functions [pos] and [neg].

   Preserves the original order of the list, i.e. each sublist is in the same order.

   For instance, [sign_combine (fun x ->"+"^x) (fun x -> "-"^x) ["1";"2"]] returns [["+1";"+2"];["+1";"-2"];["-1";"+2"];["-1";"-2"]].
*)
let rev_sign_combine ~(pos: 'a -> 'b) ~(neg : 'a -> 'b) : 'a list -> 'b list list =
  let rec aux acc revpref l =
    match l with
    | [] -> (List.rev revpref) :: acc
    | head :: tail ->
      let acc = aux acc (pos head :: revpref) tail in
      aux acc (neg head :: revpref) tail
  in
  aux [] []

let sign_combine ~pos ~neg l =
  List.rev (rev_sign_combine ~pos ~neg l)

let with_delta op value kind =
  let delta = Options.LimitDelta.get () in
  let op' = match op, delta with
    | _, 0 -> Eq
    | MinusA, _ -> Ge
    | PlusA, _ -> Le
    | _ -> assert false
  in
  if delta = 0 then
    op', Exp_builder.kinteger64 kind value
  else
    op', Exp_builder.binop op (Exp_builder.kinteger64 kind value) (Exp_builder.integer delta)

let get_bounds kind : (binop*exp) list =
  if kind = IBool then
    [(Eq,Exp_builder.zero ());(Eq,Exp_builder.one ())]
  else
    let size = Cil.bitsSizeOfInt kind in
    if Cil.isSigned kind then
      [with_delta PlusA (Cil.min_signed_number size) kind;
       with_delta MinusA (Cil.max_signed_number size) kind]
    else
      [with_delta PlusA Integer.zero kind;
       with_delta MinusA (Cil.max_unsigned_number size) kind]

let is_bound _kind _i = false
(* let size = Cil.bitsSizeOfInt kind in
   if Cil.isSigned kind then
   i>= (Cil.max_signed_number size)
   else
   i = Integer.of_int 0 || i >= Cil.max_signed_number size *)
