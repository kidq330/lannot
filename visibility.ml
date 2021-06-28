open Cil_types
open Ast_const

class to_visibility = object(self)
  inherit Visitor.frama_c_inplace
  val mutable to_add_vInfo = []
  val mutable to_add_ret = None,[]
  val unk_loc = Cil_datatype.Location.unknown

  method private mk_init ?(loc=unk_loc) vi value =
    Local_init(vi,AssignInit(SingleInit(value)),loc)
    |> Stmt_builder.instr

  method private mk_comp ?(loc=unk_loc) op vi value =
    let new_exp = Cil.new_exp ~loc (Lval (Var vi, NoOffset)) in
    Exp_builder.binop op new_exp value

  method! vfunc _ =
    Cil.DoChildrenPost (fun f ->
        let inits =
          List.map (fun (vi,_,_) ->
              self#mk_init vi (Cil.zero ~loc:unk_loc)
            ) to_add_vInfo
        in
        to_add_vInfo <- [];
        to_add_ret <- None,[];
        f.sbody.bstmts <- inits @ f.sbody.bstmts;
        f
      )

  method! vblock _ =
    Cil.DoChildrenPost (fun block ->
        match to_add_ret with
        | None, _ | Some _, [] -> block
        | Some s, lbls ->
          let rec aux l acc =
            match l with
            | [] -> acc
            | s' :: t when Cil_datatype.Stmt.equal s s' ->
              s'.skind <- Block (Cil.mkBlock (lbls @ [Stmt_builder.mk s'.skind]));
              aux t (acc @ [s'])
            | s' :: t -> aux t (acc @ [s'])
          in
          block.bstmts <- aux block.bstmts [];
          block
      )

  method! vstmt_aux s =
    match s.skind with
    | Instr (Call (_, {enode=Lval (Var {vname=name}, NoOffset)},
                   [cond;{enode=Const (CInt64 (id, IInt, None))} as idExp; tagExp], loc))
      when String.equal name "pc_label" ->
      let vname = "__SEQ_VISIBILITY_" ^ (string_of_int (Integer.to_int id)) in
      let pred_vInfo = Cil.makeVarinfo false false vname (TInt(IInt,[])) in
      let set =  Ast_info.mkassign (Var pred_vInfo, NoOffset) cond loc in
      to_add_vInfo <- to_add_vInfo @ [(pred_vInfo,idExp,tagExp)];
      s.skind <- Instr set;
      Cil.SkipChildren
    | Return (_,loc) ->
      let lbls =
        List.map (fun (vi,id,tag) ->
            let cond = self#mk_comp ~loc Ne vi (Cil.zero ~loc) in
            Utils.mk_call "pc_label" [cond; id; tag]
          ) to_add_vInfo
      in
      to_add_ret <- (Some s,lbls);
      Cil.SkipChildren
    | _ -> Cil.DoChildren

end

let to_visibility ast =
  Visitor.visitFramacFileSameGlobals (new to_visibility :> Visitor.frama_c_visitor) ast;
  Ast.mark_as_changed ()