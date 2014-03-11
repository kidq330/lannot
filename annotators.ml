open Cil_types

type annotation = int * string * exp * location

type annotator = {
  name:string;
  descr: string;
  compute: int ref -> annotation list ref -> Cil_types.file -> unit
}

module type ANNOTATOR = sig
  val name : string
  val descr : string
  val compute : (Cil_types.exp -> Cil_types.location -> Cil_types.stmt) -> Cil_types.file -> unit
end
module type ANNOTATOR_WITH_EXTRA_TAGS = sig
  val name : string
  val descr : string
  val compute : (extra:string list -> Cil_types.exp -> Cil_types.location -> Cil_types.stmt) -> Cil_types.file -> unit
end

module type S = sig
  val self : annotator
end

let annotators = Hashtbl.create 10
let nextId = ref 1

let annotate_with ?(acc=[]) ?(nextId=nextId) annotator ast =
  let acc = ref acc in
  Options.feedback "apply annotations for %s@." annotator.name;
  annotator.compute nextId acc ast;
  !acc

let annotate names ast =
  let f acc name =
    try
      annotate_with ~acc (Hashtbl.find annotators name) ast
    with Not_found ->
      Options.warning "unknown annotators `%s`" name;
      acc
  in
  List.fold_left f [] names

let mk_compute compute name nextId acc ast =
  let label_maker cond loc =
    let tag = name in
    let id = !nextId in
    incr nextId;
    acc := (id,tag,cond,loc) :: !acc;
    let tagExp = Utils.mk_exp (Const (CStr tag)) in
    let idExp = Utils.mk_exp (Const (CInt64 (Integer.of_int id, IInt, None))) in
    Utils.mk_call "pc_label" [ cond; idExp; tagExp ]
  in
  compute label_maker ast

let mk_compute_extras compute name nextId acc ast =
  let label_maker ~extra cond loc =
    let tag = String.concat " " (name::extra) in
    let id = !nextId in
    incr nextId;
    acc := (id,tag,cond,loc) :: !acc;
    let tagExp = Utils.mk_exp (Const (CStr tag)) in
    let idExp = Utils.mk_exp (Const (CInt64 (Integer.of_int id, IInt, None))) in
    Utils.mk_call "pc_label" [ cond; idExp; tagExp ]
  in
  compute label_maker ast

let register_annotator ann =
  Hashtbl.replace annotators ann.name ann;
  Options.Annotators.set_possible_values (Hashtbl.fold (fun k _v acc -> k :: acc) annotators [])

module Register (A : ANNOTATOR) = struct
  let self = { name = A.name; descr = A.descr; compute = mk_compute A.compute A.name }
  let () = register_annotator self
end

module RegisterWithExtraTags (A : ANNOTATOR_WITH_EXTRA_TAGS) = struct
  let self = { name = A.name; descr = A.descr; compute = mk_compute_extras A.compute A.name }
  let () = register_annotator self
end


let shouldInstrument fun_varinfo =
  let names = Options.FunctionNames.get () in
  (* TODO filter builtin functions *)
  if Datatype.String.Set.is_empty names then
    true
  else
    Datatype.String.Set.mem fun_varinfo.vname names

