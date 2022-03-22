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

let filen = ref ""

let get_file_name () = !filen

type annotation =
  int * string * exp * location

type annotator = {
  name:string;
  help: string;
  apply: (unit -> int) -> (annotation -> unit) -> Cil_types.file -> unit;
}

module type ANNOTATOR = sig
  val name : string
  val help : string
  val apply : (Cil_types.exp -> Cil_types.exp list -> Cil_types.location -> Cil_types.stmt) -> Cil_types.file -> unit
end
module type ANNOTATOR_WITH_EXTRA_TAGS = sig
  val name : string
  val help : string
  val apply : (extra:string list -> Cil_types.exp -> Cil_types.exp list -> Cil_types.location -> Cil_types.stmt) -> Cil_types.file -> unit
end

module type S = sig
  val self : annotator
  val apply : ?id:(unit -> int) -> ?collect:(annotation -> unit) -> Cil_types.file -> unit
end

let annotators = Hashtbl.create 10

let nextId = ref 1
let nextBindingId = ref 1
let nextIdHl = ref 1

let getCurrentLabelId () = !nextId - 1
let getCurrentBindingId () = !nextBindingId - 1
let getCurrentHLId () = !nextIdHl - 1

let next () =
  let id = !nextId in
  incr nextId;
  id
let next_binding () =
  let id = !nextBindingId in
  incr nextBindingId;
  id
let next_hl () =
  let id = !nextIdHl in
  incr nextIdHl;
  string_of_int id

let nocollect _ = ()

let () =
  Cil_builtins.add_special_builtin "pc_label";
  Cil_builtins.add_special_builtin "pc_label_bindings";
  Cil_builtins.add_special_builtin "mutated"

let pc_label = ref None
let pc_label_bindings = ref None
let mutated = ref None
let label_function_vinfo = ref None

let init_builtins () =
  pc_label := Some (Cil.makeGlobalVar "pc_label" (TFun(Cil.voidType, None, false, [])));
  pc_label_bindings := Some (Cil.makeGlobalVar "pc_label_bindings" (TFun(Cil.voidType, None, false, [])));
  mutated := Some (Cil.makeGlobalVar "mutated" (TFun(Cil.intType, None, false, [])));
  label_function_vinfo := !pc_label

let annotate_with annotator ?(id=next) ?(collect=nocollect) ast =
  Options.feedback "apply annotations for %s@." annotator.name;
  annotator.apply id collect ast

let annotate filename names ?(id=next) ?(collect=nocollect) ast =
  filen := filename;
  let f name =
    try
      let ann = Hashtbl.find annotators name in
      annotate_with ~id ~collect ann ast
    with Not_found -> Options.warning "unknown annotators `%s`" name
  in
  List.iter f names;
  if Options.Visibility.get () then Visibility.to_visibility ast !pc_label

let print_help fmt =
  let annotators = Hashtbl.fold (fun _k v acc -> v :: acc) annotators [] in
  let annotators = List.sort (fun a b -> compare a.name b.name) annotators in
  let width = List.fold_left (fun acc ann -> max (String.length ann.name) acc) 0 annotators in
  let f ann = Format.fprintf fmt "%-*s @[%s@]@." width ann.name ann.help in
  List.iter f annotators

let mk_label id collect tag cond mvars loc =
  let id = id () in
  let cond =
    if Options.Simplify.get () then
      Simplify.simplify_exp cond
    else
      cond
  in
  let tag =
    if (Option.get !label_function_vinfo).vname = "pc_label" &&
       Options.Visibility.get ()
    then "V_"^tag
    else tag
  in
  collect (id,tag,cond,loc);
  let tagExp = Ast_const.Exp_builder.mk (Const (CStr tag)) in
  let idExp = Ast_const.Exp_builder.integer id in
  Utils.mk_call !label_function_vinfo (List.concat [ [ cond; idExp; tagExp ] ; mvars])

let mk_apply apply name id collect ast =
  apply (mk_label id collect name) ast;
  Ast.mark_as_changed ()

let mk_compute_extras apply name id collect ast =
  let mk_label' ~extra cond loc=
    let tag = String.concat " " (name :: extra) in
    mk_label id collect tag cond loc
  in
  apply mk_label' ast

let register_annotator ann =
  Options.debug "register %s annotator" ann.name;
  Hashtbl.replace annotators ann.name ann

module Register (A : ANNOTATOR) = struct
  let self = { name = A.name; help = A.help; apply = mk_apply A.apply A.name }
  let apply = annotate_with self
  let () = register_annotator self
end

module RegisterWithExtraTags (A : ANNOTATOR_WITH_EXTRA_TAGS) = struct
  let self = { name = A.name; help = A.help; apply = mk_compute_extras A.apply A.name }
  let apply = annotate_with self
  let () = register_annotator self
end

let shouldInstrumentFun fun_varinfo =
  let do_names = Options.DoFunctionNames.get () in
  let skip_names = Options.SkipFunctionNames.get () in
  let f (kf : Cil_datatype.Kf.Set.elt ) =
    (Cil_datatype.Kf.vi kf).vname =  fun_varinfo.vname
  in
  not (Cil_datatype.Kf.Set.exists f skip_names) &&
  (* TODO filter builtin functions *)
  if  Cil_datatype.Kf.Set.is_empty do_names then
    not (fun_varinfo.vinline) || Options.Inline.get ()
  else
    Cil_datatype.Kf.Set.exists f do_names

let shouldInstrumentVar varinfo =
  let do_names = Options.DoVariableNames.get () in
  let skip_names = Options.SkipVariableNames.get () in
  let f (vname : Datatype.String.Set.elt ) =
    vname =  varinfo.vname
  in
  not (Datatype.String.Set.exists f skip_names) &&
  (* TODO filter builtin functions *)
  if not (Datatype.String.Set.is_empty do_names) then
    Datatype.String.Set.exists f do_names
  else true