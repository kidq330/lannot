open Ast_const
open Cil_types

let unk_loc = Cil_datatype.Location.unknown

let neg atom = Exp.lnot (Exp.copy atom)

let rec mk_expr pred =
  match pred.pred_content with
  | Pfalse -> Options.debug "False"; Exp.zero ()
  | Ptrue  -> Options.debug "True"; Exp.one ()
  | Pand (p1,p2) -> Options.debug "And"; Exp.binop LAnd (mk_expr p1) (mk_expr p2)
  | Por (p1,p2)  -> Options.debug "Or"; Exp.binop LOr (mk_expr p1) (mk_expr p2)
  | Pxor (p1,p2) -> Options.debug "Xor"; Exp.binop BOr (mk_expr p1) (mk_expr p2)
  | Pnot p -> Options.debug "Not"; Exp.lnot (mk_expr p)
  | Prel (r,t1,t2) -> Options.debug "Rel";
    let op = match r with
      | Rlt -> Lt
      | Rgt -> Gt
      | Rle -> Le
      | Rge -> Ge
      | Req -> Eq
      | Rneq -> Ne
    in
    Exp.binop op (!Db.Properties.Interp.term_to_exp ~result:None t1) (!Db.Properties.Interp.term_to_exp ~result:None t2)
  | _ -> Options.feedback "todo" ;Exp.zero ()



let gen_labels_assert mk_label (pred:Cil_types.predicate) =
  let loc = pred.pred_loc in
  let exp = neg (mk_expr pred) in
  let l1 = mk_label exp [] loc in
  Stmt.block [l1]


(** Assert coverage **)
class visitorAssert mk_label = object(_)
  inherit Visitor.frama_c_inplace

  val mutable labellist = []

  method! vstmt_aux stmt =
    ignore stmt;
    let add_labels stmt =
      let tmp = labellist in
      labellist <- [];
      (Stmt.block (tmp @ [ stmt ]))
    in
    Cil.DoChildrenPost add_labels

  method! vcode_annot annot =
    begin match annot.annot_content with
      | AAssert (_, p) ->
        let exp = gen_labels_assert mk_label p in
        labellist <- labellist @ [exp];
        Cil.DoChildren
      | _ ->
        Cil.DoChildren
    end

end

(**
   Assert coverage annotator
*)
module AssertCov = Annotators.Register (struct
    let name = "ASSERT"
    let help = "Assert Coverage"
    let apply mk_label file =
      Visitor.visitFramacFileSameGlobals (new visitorAssert mk_label :> Visitor.frama_c_visitor) file
  end)
