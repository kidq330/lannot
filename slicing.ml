open Cil_types


(* val slice : stmt list -> string -> Project.t *)
(*
let slice stmts prj_name =
  let _ = !Db.Slicing.set_modes ~keepAnnotations:true () in
  let slicing = !Db.Slicing.Project.mk_project prj_name in
  let select sel stmt = !Db.Slicing.Select.select_stmt sel ~spare:false stmt
    (Kernel_function.find_englobing_kf stmt) in
  let sel = List.fold_left select Db.Slicing.Select.empty_selects stmts in
  let _ = !Db.Slicing.Request.add_persistent_selection slicing sel in
  let _ = !Db.Slicing.Request.apply_all_internal slicing in
  let _ = !Db.Slicing.Slice.remove_uncalled slicing in
  !Db.Slicing.Project.extract prj_name slicing
*)
let getProject stmts prj_name =
  let prj = File.create_project_from_visitor prj_name (fun prj -> new Visitor.frama_c_copy prj) in 
    prj

(* val tested_func: unit -> string *)
let tested_func () =
  match (fst (Globals.entry_point ())).fundec with
    | Definition(f,_) -> f.svar.vname
    | Declaration _ -> failwith "entry_point"


(* val print_alarms: stmt list -> unit *)
let print_alarms l =
  let s = List.map(fun x -> Printf.sprintf "%i "(Utils.get_stmt_loc_int x)) l in
  let str = List.fold_left (^) "" s in
  Options.Self.debug ~level:1 "%s" str


(* val impacted_stmts: stmt list -> stmt list list *)
(* returns the impacted stmts for each stmt *)
let impacted_stmts stmts =
  List.map (fun x ->
    let l = !Db.Impact.from_stmt x in
    List.filter (fun y -> List.mem y stmts && not (Utils.same_line x y)) l
  ) stmts


(* val final_stmts: stmt list -> stmt list *)
(* if min equals true, returns only one final stmt per equivalence class *)
let final_stmts ?min:(m = false) stmts =
  let imp = impacted_stmts stmts in
  let rec is_final s = function
    | [] -> true
    | h::t ->
      let impact = !Db.Impact.from_stmt h in
      if List.mem s impact then
	is_final s t
      else
	false in
  let rec aux ret l1 l2 = match (l1, l2) with
    | [],[] -> ret
    | h1::t1, h2::t2 when is_final h1 h2 ->
      if m && (List.exists (fun x ->
	let impact = !Db.Impact.from_stmt x in
	List.mem h1 impact
      ) ret) then
	aux ret t1 t2
      else
	aux (h1::ret) t1 t2
    | _::t1, _::t2 -> aux ret t1 t2
    | _ -> failwith "final_stmts" in
  aux [] stmts imp



module type Type = sig
  val process : stmt list -> Verdict.t list
  val name : string
end


(*************************)
(* NONE                  *)
(*************************)

module None:Type = struct
  let name = "none"
  let process _ =
    let prj = getProject !Utils.all_stmts (Config.input_file()) in
    let filename = (Project.get_name prj) ^ "_labels.c" in
      Instru.generate_labels_prj prj;
      let _ = Instru.print_project prj filename in
	[]

end


module Multi:Type = struct
  let name = "multi"
  let process _ =
    let prj = getProject !Utils.all_stmts (Config.input_file()) in
    let filename = (Project.get_name prj) ^ "_multilabels.c" in
      Instru.generate_multi_labels_prj prj;
      let _ = Instru.print_project prj filename in
	[]

end

module Aor:Type = struct
  let name = "aor"
  let process _ =
    let prj = getProject !Utils.all_stmts (Config.input_file()) in
      let _ = Instru.generate_aor_mutants prj in
	[]
end


module Ror:Type = struct
  let name = "ror"
  let process _ =
    let prj = getProject !Utils.all_stmts (Config.input_file()) in
    let _ = Instru.generate_ror_mutants prj in
      []
end

module Cor:Type = struct
  let name = "cor"
  let process _ =
    let prj = getProject !Utils.all_stmts (Config.input_file()) in
    let _ = Instru.generate_cor_mutants prj in
      []
end
