open Cil_types
open Utils

module type CONFIG = sig
  val n : int
  val all_boolean : bool
  val mk_label : exp -> location -> stmt
end
;;

let pos atom = Cil.copy_exp atom
let neg atom = Utils.mk_exp (UnOp (LNot, Cil.copy_exp atom, Cil.intType))


module GenericConditionCoverage (C : CONFIG) = struct
  open C

  (**
    Generates labels from a boolean expression.
    Adds label statements in front of the provided accumalator.
  *)
  let gen_labels (acc : stmt list) (bexpr : exp) : stmt list =
    let loc = bexpr.eloc in
    let atoms = atomic_conditions bexpr in
    let m = if n <= 0 then List.length atoms else n in
    Options.debug3 "%d atoms in @[b%a@]" (List.length atoms) Cil_printer.pp_exp bexpr;

    (* Compute subsets of m atoms *)
    let subsets = rev_combine m atoms in
    Options.debug2 "%d subsets of %d atoms" (List.length subsets) m;

    let for_signed_subset (acc : stmt list) (signed_subset : exp list) : stmt list =
      let exp = andify signed_subset in
      mk_label exp loc :: acc
    in
    let for_subset (acc : stmt list) (subset : exp list) : stmt list =
      (* For each subset of atoms, compute every sign combination *)
      let signed_subsets = rev_sign_combine pos neg subset in
      List.fold_left for_signed_subset acc signed_subsets
    in
    List.fold_left for_subset acc subsets;;

  class visitor = object(self)
    inherit Visitor.frama_c_inplace

    val mutable bexprs = []

    method! vfunc dec =
      if Annotators.shouldInstrument dec.svar then
        Cil.DoChildren
      else
        Cil.SkipChildren

    method vstmt_post stmt =
      match bexprs with
      | [] -> stmt
      | _ ->
        let stmts = List.fold_left gen_labels [stmt] bexprs in
        bexprs <- [];
        Cil.mkStmt (Block (Cil.mkBlock stmts))

    method! vstmt_aux stmt =
      match stmt.skind with
      | If (e, thenb, elseb, loc) ->
        let stmts = gen_labels [stmt] e in
        (* handle visits manually to skip visit of e *)
        let thenb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) thenb in
        let elseb = Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) elseb in
        stmt.skind <- If (e, thenb, elseb, loc);
        Cil.ChangeTo (Cil.mkStmt (Block (Cil.mkBlock stmts)))
      | _ ->
        if all_boolean then
          Cil.DoChildrenPost (fun stmt -> self#vstmt_post stmt)
        else
          Cil.DoChildren

    method! vexpr expr =
      if all_boolean && is_boolean expr then begin
        bexprs <- expr :: bexprs;
        Cil.SkipChildren
      end else
        Cil.DoChildren
  end

  let compute file =
    Visitor.visitFramacFileSameGlobals (new visitor :> Visitor.frama_c_visitor) file

end

module NCC = Annotators.Register (struct
  let name = "NCC"
  let help = "n-condition coverage"

  let compute mk_label file =
    let n = Options.N.get () in
    let all_boolean = Options.AllBoolExps.get () in
    Options.debug2 "n-condition coverage config: n=%d, all booleans=%B" n all_boolean;
    let module G = GenericConditionCoverage (struct
      let n = n
      let all_boolean = all_boolean
      let mk_label = mk_label
    end) in
    G.compute file

end)

