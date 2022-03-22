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
open Utils
open Ast_const
open Exp_builder

let array_to_string l r = l ^ ",\n" ^ r

(**
   Generate labels for n-CC coverage from a Boolean expression.
   And puts them all in a single block statement.
*)
let gen_labels_ncc mk_label n (bexpr : exp) : stmt =
  let loc = bexpr.eloc in
  let atoms = atomic_conditions bexpr in
  let natoms = List.length atoms in
  Options.debug "%d atoms in @[%a@]" natoms Printer.pp_exp bexpr;

  (* Compute subsets of m atoms *)
  let n = if n <= 0 then natoms else min n natoms in
  let subsets = combine n atoms in
  Options.debug "%d subsets of %d atoms" (List.length subsets) n;

  (* For each signed subset of atoms, *)
  let for_signed_subset (acc : stmt list) (signed_subset : exp list) : stmt list =
    (* Get conjunction as an expression*)
    let exp = join LAnd signed_subset in
    (* Create a label and put it in front of acc *)
    mk_label exp [] loc :: acc
  in

  (* For each subset of atoms, *)
  let for_subset (acc : stmt list) (subset : exp list) : stmt list =
    (* Compute signed subsets *)
    let signed_subsets = sign_combine (fun id -> id) lnot subset in
    (* Create labels for each signed subset (taken in rev. order)
       and put them in rev. in front of acc (N.B. [rev rev l = l]) *)
    List.fold_left for_signed_subset acc signed_subsets
  in
  List.rev (List.fold_left for_subset [] subsets)
  |> Stmt_builder.block

(** Generate DC labels for the given Boolean formula *)
let gen_labels_dc mk_label bexpr =
  let loc = bexpr.eloc in
  let l1 = mk_label bexpr [] loc in
  let l2 = mk_label (lnot bexpr) [] loc in
  Stmt_builder.block [l1;l2]

(** Generate GACC labels for one particular active clause *)
let gen_labels_gacc_for mk_label whole part =
  let loc = whole.eloc in
  let w0 = replace whole part (one ()) in
  let w1 = replace whole part (zero ()) in

  (* rather than to test w0 != w1, do (w0 && !w1) || (!w0 && w1) *)
  let indep = niff w0 w1 in

  let a_indep = binop LAnd part indep in
  let na_indep = binop LAnd (lnot part) indep in

  List.map (fun e -> mk_label e [] loc) [a_indep; na_indep]

let hlab_cacc = ref [| |]

(** Generate GACC labels for the given Boolean formula *)
let gen_labels_gacc mk_label bexpr =
  atomic_conditions bexpr
  |> List.map (gen_labels_gacc_for mk_label bexpr)
  |> List.flatten
  |> Stmt_builder.block

(** Generate CACC labels for one particular active clause *)
let gen_labels_cacc_for mk_label whole part =
  Annotators.label_function_vinfo := !Annotators.pc_label_bindings;
  let loc = whole.eloc in
  let w0 = replace whole part (one ()) in
  let w1 = replace whole part (zero ()) in

  let binding_id = Annotators.next_binding () in

  (* rather than to test w0 != w1, do (w0 && !w1) || (!w0 && w1) *)
  let indep = niff w0 w1 in

  let a_indep = binop LAnd part (Cil.copy_exp indep) in
  let na_indep = binop LAnd (lnot part) (Cil.copy_exp indep) in

  let l = mk_label a_indep [integer binding_id; integer 1 ; mk (Const (CStr "pa")) ; whole] loc in
  let idl = Annotators.getCurrentLabelId () in
  let r = mk_label na_indep [integer binding_id; integer 1 ; mk (Const (CStr "pb")) ; whole] loc in
  let idr = Annotators.getCurrentLabelId () in

  hlab_cacc := Array.append !hlab_cacc [| (idl,idr) |];
  Annotators.label_function_vinfo := !Annotators.pc_label;
  [ l ; r ]

(** Generate CACC labels for the given Boolean formula *)
let gen_labels_cacc mk_label bexpr =
  atomic_conditions bexpr
  |> List.map (gen_labels_cacc_for mk_label bexpr)
  |> List.flatten
  |> Stmt_builder.block

(** Generate CACC hyperlabels *)
let couple_to_string c = Annotators.next_hl() ^ ") <l" ^ (string_of_int (fst c)) ^ ".l" ^ (string_of_int (snd c)) ^ "|;pa!=pb;>"

let store_hyperlabel_data out str =
  let formatter = Format.formatter_of_out_channel out in
  Format.fprintf formatter "%s@." str

let gen_hyperlabels_cacc = ref (fun () ->
    let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
    Options.feedback "write hyperlabel data (to %s)" data_filename;
    let out = open_out_gen [Open_creat; Open_append] 0o644 data_filename in
    store_hyperlabel_data out  (Array.fold_right array_to_string (Array.map couple_to_string !hlab_cacc) "");
    close_out out)



let hlab_racc = ref [| |]

let handle_list_l la a = List.concat [ la ; [  mk (Const (CStr ("cA" ^ (string_of_int ((List.length la) / 2 + 1))))) ; a ] ]
let handle_list_r la a = List.concat [ la ; [  mk (Const (CStr ("cB" ^ (string_of_int ((List.length la) / 2 + 1))))) ; a ] ]

(** Generate RACC labels for one particular active clause *)
let gen_labels_racc_for mk_label whole atoms part =
  Annotators.label_function_vinfo := !Annotators.pc_label_bindings;
  let loc = whole.eloc in
  let w0 = replace whole part (one ()) in
  let w1 = replace whole part (zero ()) in

  let binding_id = Annotators.next_binding () in

  (* rather than to test w0 != w1, do (w0 && !w1) || (!w0 && w1) *)
  let indep = niff w0 w1 in

  let a_indep = binop LAnd part indep in
  let na_indep = binop LAnd (lnot part) indep in

  let atoms_without_current = List.filter (fun a -> part <> a) atoms in
  let l = mk_label a_indep (List.concat [[integer binding_id; integer (List.length atoms_without_current)] ; List.fold_left handle_list_l [] atoms_without_current]) loc in
  let idl = Annotators.getCurrentLabelId () in
  let r = mk_label na_indep (List.concat [[integer binding_id; integer (List.length atoms_without_current)] ; List.fold_left handle_list_r [] atoms_without_current]) loc in
  let idr = Annotators.getCurrentLabelId () in

  hlab_racc := Array.append !hlab_racc [| (idl,(idr,(List.length atoms_without_current))) |];
  Annotators.label_function_vinfo := !Annotators.pc_label;
  [ l ; r ]

(** Generate RACC labels for the given Boolean formula *)
let gen_labels_racc mk_label bexpr =
  let atoms = atomic_conditions bexpr in
  List.map (gen_labels_racc_for mk_label bexpr atoms) atoms
  |> List.flatten
  |> Stmt_builder.block

(** Generate RACC hyperlabels *)
let rec generate_equalities i =
  match i with
  | 0 -> " "
  | 1 -> "cA1 == cB1"
  | _ -> (generate_equalities (i-1)) ^ " && " ^ "cA" ^ (string_of_int i)  ^ "== cB" ^ (string_of_int i)

let couple_to_string c =
  Annotators.next_hl() ^ ") <l"
  ^ (string_of_int (fst c)) ^ ".l"
  ^ (string_of_int (fst (snd c)))
  ^ "|;"
  ^ generate_equalities (snd (snd c))
  ^ ";>"

let gen_hyperlabels_racc = ref (fun () ->
    let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
    Options.feedback "write hyperlabel data (to %s)" data_filename;
    let out = open_out_gen [Open_creat; Open_append] 0o644 data_filename in
    store_hyperlabel_data out  (Array.fold_right array_to_string (Array.map couple_to_string !hlab_racc) "");
    close_out out)




(** Generate GICC labels for the given Boolean formula *)
let gen_labels_gicc_for mk_label whole part =
  let loc = whole.eloc in
  (* Compute negative and positive Shannon's factors wrt to part *)
  let factor0 = replace whole part (one ()) in
  let factor1 = replace whole part (one ()) in
  (* Check the inactivity of part *)
  let inactive = iff factor0 factor1 in

  let true_inactive = binop LAnd part inactive in
  let false_inactive = binop LAnd (lnot part) inactive in
  let true_inactive_true = binop LAnd true_inactive whole in
  let true_inactive_false = binop LAnd true_inactive (lnot whole) in
  let false_inactive_true = binop LAnd false_inactive whole in
  let false_inactive_false = binop LAnd false_inactive (lnot whole) in
  List.map (fun e -> mk_label e [] loc) [
      true_inactive_true;
      true_inactive_false;
      false_inactive_true;
      false_inactive_false;
    ]

(** Generate GICC labels for the given Boolean formula *)
let gen_labels_gicc mk_label bexpr =
  atomic_conditions bexpr
  |> List.map (gen_labels_gicc_for mk_label bexpr)
  |> List.flatten
  |> Stmt_builder.block

(** Visotor that will store all limits expressions *)
class visitExp = object(self)
  inherit Visitor.frama_c_inplace
  val mutable bexprs = []
  method get_exprs () = bexprs

  method private mk_limit cond exp =
    (* Example
       cond : a < b
       exp : (a - b) + 1
       posComp : exp <= int_delta
       negComp : -exp <= int_delta
       abs : posComp && negComp
       ret: cond && abs
       cf. COQ proof in file LIMIT_proof.v
    *)
    let delta = Options.LimitDelta.get () in
    let posComp = binop Le exp (integer delta) in
    let negComp = binop Le (neg exp) (integer delta) in
    let abs = binop LAnd posComp negComp in
    binop LAnd cond abs

  method! vexpr cond =
    match cond.enode with
    | BinOp ((Lt | Le | Gt | Ge) as op, e1, e2, _) ->
      ignore (Visitor.visitFramacExpr (self :> Visitor.frama_c_visitor) e1);
      ignore (Visitor.visitFramacExpr (self :> Visitor.frama_c_visitor) e2);
      let e = binop MinusA e1 e2 in
      let isInt = Cil.isIntegralType (Cil.typeOf e) in
      if isInt then begin
        begin match op with
          | Lt ->
            let to_zero = binop PlusA e (one()) in
            let new_exp = self#mk_limit cond to_zero in
            bexprs <- new_exp :: bexprs
          | Gt ->
            let to_zero = binop MinusA e (one()) in
            let new_exp = self#mk_limit cond to_zero in
            bexprs <- new_exp :: bexprs
          | Le | Ge ->
            let new_exp = self#mk_limit cond e in
            bexprs <- new_exp :: bexprs
          | _ -> ()
        end
      end;
      Cil.SkipChildren
    | _ -> Cil.DoChildren
end

(** Generate Limit labels for the given Boolean formula *)
let gen_labels_limit mk_label bexpr =
  let loc = bexpr.eloc in
  let ve = new visitExp in
  ignore (Visitor.visitFramacExpr (ve :> Visitor.frama_c_visitor) bexpr);
  List.map (fun exp -> mk_label exp [] loc) (ve#get_exprs())
  |> Stmt_builder.block

(**
   Frama-C in-place visitor that injects labels at each condition/boolean
   expression using some injection function
*)
class visitor gen_labels all_boolean = object(self)
  inherit Visitor.frama_c_inplace

  val mutable bexprs = []

  method! vfunc dec =
    if Annotators.shouldInstrumentFun dec.svar then
      Cil.DoChildren
    else
      Cil.SkipChildren

  method! vstmt_aux stmt =
    match stmt.skind with
    | If (e, thenb, elseb, loc) ->
      let labels_stmt = gen_labels e in
      (* handle visits manually to skip visit of e *)
      let thenb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) thenb in
      let elseb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) elseb in
      stmt.skind <- Block (Cil.mkBlock [labels_stmt; Stmt_builder.mk (If (e, thenb, elseb, loc))]);
      Cil.SkipChildren
    | _ ->
      if all_boolean then
        Cil.DoChildrenPost (fun stmt ->
            match bexprs with
            | [] -> stmt
            | _ ->
              let labels = List.rev_map gen_labels bexprs in
              bexprs <- [];
              stmt.skind <- Block (Cil.mkBlock (labels @ [Stmt_builder.mk stmt.skind]));
              stmt
          )
      else
        Cil.DoChildren

  method! vinst instr =
    if is_label instr then Cil.SkipChildren
    else Cil.DoChildren

  method! vexpr expr =
    if all_boolean && is_boolean expr then begin
      bexprs <- expr :: bexprs;
      Cil.SkipChildren
    end else
      Cil.DoChildren
end

(** Generic condition/boolean expression annotator *)
let apply gen_labels all_boolean file =
  Visitor.visitFramacFileSameGlobals (new visitor gen_labels all_boolean :> Visitor.frama_c_visitor) file


(** n-CC condition/boolean expression annotator *)
let apply_ncc mk_label n all_boolean file =
  Options.debug "n-Condition Coverage config: n=%d, all booleans=%B" n all_boolean;
  apply (gen_labels_ncc mk_label n) all_boolean file


(**
   Condition coverage annotator, special case of n-CC for n=1
*)
module CC = Annotators.Register (struct
    let name = "CC"
    let help = "Condition Coverage"

    let apply mk_label file =
      apply_ncc mk_label 1 (Options.AllBoolExps.get ()) file
  end)

(**
   n-wise condition coverage annotator
*)
module NCC = Annotators.Register (struct
    let name = "NCC"
    let help = "n-wise Condition Coverage"

    let apply mk_label file =
      apply_ncc mk_label (Options.N.get ()) (Options.AllBoolExps.get ()) file
  end)

(**
   Multiple condition coverage annotator, special case of n-CC for n=infinite
   (coded zero)
*)
module MCC = Annotators.Register (struct
    let name = "MCC"
    let help = "Multiple Condition Coverage"

    let apply mk_label file =
      apply_ncc mk_label 0 (Options.AllBoolExps.get ()) file
  end)

(**
   Decision Coverage annotator
*)
module DC = Annotators.Register (struct
    let name = "DC"
    let help = "Decision Coverage"

    let apply mk_label file =
      apply (gen_labels_dc mk_label) (Options.AllBoolExps.get ()) file
  end)

(**
   General Active Clause Coverage annotator
*)
module GACC = Annotators.Register (struct
    let name = "GACC"
    let help = "General Active Clause Coverage (weakened MCDC)"

    let apply mk_label file =
      apply (gen_labels_gacc mk_label) (Options.AllBoolExps.get ()) file
  end)


(**
   Correlated Active Clause Coverage annotator
*)
module CACC = Annotators.Register (struct
    let name = "CACC"
    let help = "Correlated Active Clause Coverage (masking MCDC)"
    let apply mk_label file =
      apply (gen_labels_cacc mk_label) (Options.AllBoolExps.get ()) file;
      !gen_hyperlabels_cacc ()
  end)


(**
   Restricted Active Clause Coverage annotator
*)
module RACC = Annotators.Register (struct
    let name = "RACC"
    let help = "Restricted Active Clause Coverage (strong MCDC)"
    let apply mk_label file =
      apply (gen_labels_racc mk_label) (Options.AllBoolExps.get ()) file;
      !gen_hyperlabels_racc ()
  end)



(**
   General Inactive Clause Coverage annotator
*)
module GICC = Annotators.Register (struct
    let name = "GICC"
    let help = "General Inactive Clause Coverage"
    let apply mk_label file =
      apply (gen_labels_gicc mk_label) (Options.AllBoolExps.get ()) file

  end)


(**
   Limit Coverage annotator
*)
module Limit = Annotators.Register (struct
    let name = "LIMIT"
    let help = "Limit Coverage"
    let apply mk_label file =
      apply (gen_labels_limit mk_label) (Options.AllBoolExps.get ()) file
  end)
