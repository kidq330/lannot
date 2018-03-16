open Ast_const
open Cil_types

let unk_loc = Cil_datatype.Location.unknown

let neg atom = Exp.lnot (Exp.copy atom)

let predicate_to_exp = Dynamic.get ~plugin:"E_ACSL" "predicate_to_exp"
    (Datatype.func2
       Kernel_function.ty Cil_datatype.Predicate.ty Cil_datatype.Exp.ty)
    
let rec mk_expr pred =
  match pred.pred_content with
  | Pfalse -> Options.debug "False"; Exp.zero ()
  | Ptrue  -> Options.debug "True"; Exp.one ()
  | Papp _ -> Options.feedback "todo App" ;Exp.zero ()
  | Pseparated _ -> Options.feedback "todo Separated" ;Exp.zero ()
  | Prel (r,t1,t2) ->
    let op = match r with
      | Rlt -> Options.debug "Rel Rlt"; Lt
      | Rgt -> Options.debug "Rel Rgt";Gt
      | Rle -> Options.debug "Rel Rle";Le
      | Rge -> Options.debug "Rel Rge";Ge
      | Req -> Options.debug "Rel Req";Eq
      | Rneq -> Options.debug "Rel Rneq";Ne
    in
    Exp.binop op (!Db.Properties.Interp.term_to_exp ~result:None t1) (!Db.Properties.Interp.term_to_exp ~result:None t2)
  | Pand (p1,p2) -> Options.debug "And"; Exp.binop LAnd (mk_expr p1) (mk_expr p2)
  | Por (p1,p2)  -> Options.debug "Or"; Exp.binop BOr (mk_expr p1) (mk_expr p2)
  | Pxor (p1,p2) ->
    Options.debug "Xor";
    let e1 =  (mk_expr p1) in
    let e2 =  (mk_expr p2) in
    Exp.binop  LAnd (Exp.binop LOr e1 e2) (Exp.lnot (Exp.binop LAnd e1 e2))
  | Pimplies (p1,p2) ->
    Options.feedback "Implies" ;
    let e1 =  (mk_expr p1) in
    let e2 =  (mk_expr p2) in
    Exp.implies e1 e2
  | Piff (p1,p2) ->
    Options.feedback "Iff" ;
    let e1 =  (mk_expr p1) in
    let e2 =  (mk_expr p2) in
    Exp.iff e1 e2
  | Pnot p -> Options.debug "Not"; Exp.lnot (mk_expr p)
  | Pif (t,p1,p2) ->
    Options.feedback "If" ;
    let c = (!Db.Properties.Interp.term_to_exp ~result:None t) in
    let e1 =  (mk_expr p1) in
    let e2 =  (mk_expr p2) in
    Exp.binop  LOr (Exp.binop LAnd c e1) (Exp.binop LAnd (Exp.lnot c) e2)
  | Plet _ -> Options.feedback "todo Let" ;Exp.zero ()
  | Pforall _ -> Options.feedback "todo Forall" ;Exp.zero ()
  | Pexists _ -> Options.feedback "todo Exists" ;Exp.zero ()
  | Pat _ -> Options.feedback "todo At" ;Exp.zero ()
  | Pvalid_read _ -> Options.feedback "todo Valid_read" ;Exp.zero ()
  | Pvalid _ -> Options.feedback "todo Valid" ;Exp.zero ()
  | Pvalid_function _ -> Options.feedback "todo Valid_function" ;Exp.zero ()
  | Pinitialized _ -> Options.feedback "todo Intitialized" ;Exp.zero ()
  | Pdangling _ -> Options.feedback "todo Dangling" ;Exp.zero ()
  | Pallocable _ -> Options.feedback "todo Allocable" ;Exp.zero ()
  | Pfreeable _ -> Options.feedback "todo Freeable" ;Exp.zero ()
  | Pfresh _ -> Options.feedback "todo Fresh" ;Exp.zero ()
  | Psubtype _ -> Options.feedback "todo Subtype" ;Exp.zero ()



let gen_labels_assert mk_label _ (pred:Cil_types.predicate) =
  let loc = pred.pred_loc in
  let exp = neg (mk_expr pred) in
  let l1 = mk_label exp [] loc in
  l1


(** Assert coverage **)
class visitorAssert mk_label = object(self)
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

        begin match self#current_kf with
          | None -> Cil.DoChildren
          | Some kf ->
            let exp = gen_labels_assert mk_label kf p in
            labellist <- labellist @ [exp];
            Cil.DoChildren
        end
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
