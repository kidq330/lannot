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

let filen = ref ""

let get_file_name () = !filen

let assertDone = ref false


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

let getCurrentLabelId () = !nextId - 1

let next () =
  let id = !nextId in
  incr nextId;
  id

let nocollect _ = ()

let incomp = Hashtbl.create 10

let () =
  Hashtbl.add incomp "ASSERT" ["ALL"]

let is_compatible name previousAnn =
  if previousAnn != [] then begin
    if List.exists (fun s -> String.equal s "ASSERT") previousAnn then
      Some("ASSERT")
    else if Hashtbl.mem incomp name then begin
      try
        let l = Hashtbl.find incomp name in
        if String.equal (List.nth l 0) "ALL" then
          Some("ALL")
        else
          let s = List.find (fun a -> List.exists (fun a2 -> String.equal a a2) l) previousAnn in
          Some(s)
      with Not_found -> None
    end
    else
      None
  end
  else
    None

let annotate_with annotator ?(id=next) ?(collect=nocollect) ast =
  Options.feedback "apply annotations for %s@." annotator.name;
  annotator.apply id collect ast;
  if String.equal annotator.name "ASSERT" then
    assertDone := true

let annotate filename names ?(id=next) ?(collect=nocollect) ast =
  filen := filename;
  let previousAnn = ref [] in
  let f name =
    match is_compatible name !previousAnn with
    | None ->
      let ann = Hashtbl.find_opt annotators name in
      begin match ann with
        | None ->
          Options.warning "unknown annotators `%s`" name
        | Some(ann) ->
          annotate_with ~id ~collect ann ast;
          previousAnn := name :: !previousAnn
      end
    | Some s ->
      Options.warning "Annotator '%s' ignored due to imcompatibility with a previous annotator %s" name s;
  in
  List.iter f names

let print_help fmt =
  let annotators = Hashtbl.fold (fun _k v acc -> v :: acc) annotators [] in
  let annotators = List.sort (fun a b -> compare a.name b.name) annotators in
  let width = List.fold_left (fun acc ann -> max (String.length ann.name) acc) 0 annotators in
  let f ann = Format.fprintf fmt "%-*s @[%s@]@." width ann.name ann.help in
  List.iter f annotators

let print_help_incomp fmt =
  let incompList = Hashtbl.fold (fun k v acc -> (k,v) :: acc) incomp [] in
  let incompList = List.sort (fun (ka,_) (kb,_) -> compare ka kb) incompList in
  let incompList = List.map (fun (ka,a) ->
      let na = String.concat " ; " a in
      (ka,"["^na^"]")
    ) incompList in
  let width = List.fold_left (fun acc (ka,_) -> max (String.length ka) acc) 0 incompList in
  let f (ka,a) = Format.fprintf fmt "%-*s @[%s@]@." width ka a in
  List.iter f incompList

let label_function_name = ref "pc_label"

let mk_label id collect tag cond mvars loc =
  let id = id () in
  let cond =
    if Options.Simplify.get () then
      Simplify.simplify_exp cond
    else
      cond
  in
  collect (id,tag,cond,loc);
  let tagExp = Exp.mk (Const (CStr tag)) in
  let idExp = Exp.integer id in
  Utils.mk_call !label_function_name (List.concat [ [ cond; idExp; tagExp ] ; mvars])

let mk_apply apply name id collect ast =
  apply (mk_label id collect name) ast

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

let shouldInstrument fun_varinfo =
  let names = Options.FunctionNames.get () in
  let f (kf : Cil_datatype.Kf.Set.elt ) =
    (Cil_datatype.Kf.vi kf).vname =  fun_varinfo.vname
  in
  (* TODO filter builtin functions *)
  (*Cil_datatype.Kf.Set.is_empty names || Cil_datatype.Kf.Set.exists f names*)
  (* Modif pour polarSSL *)
  let inlineNames = Options.InlineException.get () in
  if  Cil_datatype.Kf.Set.is_empty names then begin
    if (not fun_varinfo.vinline || Options.Inline.get ()) then
      true
    else begin
      if Cil_datatype.Kf.Set.is_empty inlineNames then
        false
      else
        Cil_datatype.Kf.Set.exists f inlineNames
    end
  end
  else
    Cil_datatype.Kf.Set.exists f names
