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

(** RCC Visitor **)
class visitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  val unk_loc = Cil_datatype.Location.unknown

  (* Used to know if we are inside an inlined block *)
  val is_inlined_block = Stack.create ()

  (* List of last temporary variables used to store mutations
     for each critical block *)
  val seen_vinfos = Stack.create ()

  (* Remember if a macro double was seen for the next if *)
  val mutable seen_double : bool = false
  (* Remember if a macro ignore was seen for the next if *)
  val mutable seen_ignore : bool = false
  (* Id used to create temporary variable names *)
  val mutable id : int = 0

  val mutable to_add = []

  (* Get the next id to use *)
  method private next () : int =
    id <- id + 1; id

  val start_inline = "__LANNOTATE_START_INLINE"
  val end_inline = "__LANNOTATE_END_INLINE"
  val start = "__CM_START"
  val end_crit = "__CM_END"
  val double_if = "__CM_DOUBLE_IF"
  val ignore_if = "__CM_IGNORE_IF"
  val target = "__CM_TARGET"

  method private in_crit_zone () =
    not (Stack.is_empty seen_vinfos) && Stack.is_empty is_inlined_block

  method private is_lannotate_builtin g =
    match g with
    | GFunDecl (_, vi, _ ) ->
      List.exists (fun builtin -> String.equal vi.vname builtin)
        [start;double_if;target;start_inline;end_inline]
    | _ -> false


  (* Creates a new temporary variable and adds it to seen_vinfos *)
  method private get_new_tmp_var () : varinfo =
    let kf = Option.get self#current_kf in
    let fdec = Kernel_function.get_definition kf in
    let name = "lannot_mut_"^string_of_int (self#next()) in
    let vi = Cil.makeTempVar ~name fdec Cil.intType in
    let fst = Stack.pop seen_vinfos in
    Stack.push (vi::fst) seen_vinfos;
    to_add <- vi :: to_add;
    vi

  (* For a given if expression, creates the corresponding mutated expression
     of the form (mut_exp && !e || !mut_exp && e), and assigns the temporary
     variable with mutate function (cf. Printer in utils.ml)
  *)
  method private generate_exp (exp: exp) : (stmt*exp) =
    let mutate_lval = Cil.var (self#get_new_tmp_var ()) in
    let loc = exp.eloc in
    let mutate_exp = Cil.new_exp ~loc (Lval mutate_lval) in
    let mutation_side = Cil.mkBinOp ~loc LAnd mutate_exp (Exp_builder.lnot exp) in
    let no_mutation_side = Cil.mkBinOp ~loc LAnd (Exp_builder.lnot mutate_exp) exp in
    let mutate_call = Utils.mk_call ~result:mutate_lval !Annotators.mutated [] in
    mutate_call, Cil.mkBinOp ~loc LOr mutation_side no_mutation_side

  (* Depending on seen_double value, changes the form of the mutated expression
     and returns it along with assigns of temporary variables *)
  method private generate_if_exp (exp:exp) : (stmt list*exp) =
    match exp.enode with
    | BinOp (LAnd|LOr as op, exp1, exp2, _) when seen_double ->
      let mutate_call, new_exp1 = self#generate_exp exp1 in
      let mutate_call',new_exp2 = self#generate_exp exp2 in
      let concat_exp = Cil.mkBinOp exp.eloc op new_exp1 new_exp2 in
      [mutate_call;mutate_call'],concat_exp
    | _ ->
      if seen_double then begin
        Options.warning "If at %a wrongly marked as double.\ Parsing can split \
                         if statement if it contains side effects. \
                         Ignoring macro..." Printer.pp_location exp.eloc;
      end;
      let mutate_call, new_exp = self#generate_exp exp in
      [mutate_call], new_exp

  (* Creates a disjunction of all seen_vinfos *)
  method private generate_disj (loc:location) (vinfos:varinfo list) =
    let rec aux vinfos acc =
      match vinfos, acc with
      | [], Some acc -> acc
      | vInfo :: tl, None ->
        let new_exp = Cil.new_exp ~loc (Lval (Cil.var vInfo)) in
        aux tl (Some(new_exp))
      | vInfo :: tl, Some acc ->
        let new_exp = Cil.new_exp ~loc (Lval (Cil.var vInfo)) in
        let new_disj = Cil.mkBinOp ~loc LOr new_exp acc in
        aux tl (Some(new_disj))
      | _ -> assert false
    in
    aux vinfos None

  (* Clears all parameters after each function *)
  method! vfunc dec =
    if not @@ Annotators.shouldInstrumentFun dec.svar then
      Cil.SkipChildren
    else
      Cil.DoChildrenPost( fun fdec ->
          Stack.clear is_inlined_block;
          let f vi =
            let zero_init = Cil.makeZeroInit ~loc:unk_loc vi.vtype in
            let local_init = AssignInit zero_init in
            let instr_init = Local_init(vi, local_init, unk_loc) in
            vi.vdefined <- true;
            Stmt_builder.instr instr_init
          in
          let inits = List.rev @@ List.map f to_add in
          fdec.sbody.bstmts <- inits @ fdec.sbody.bstmts;
          Stack.clear seen_vinfos;
          to_add <- [];
          seen_double <- false;
          seen_ignore <- false;
          fdec
        )

  (* Handles inlined block to avoid annotating them *)
  method! vblock b =
    if not (Options.InlinedBlock.get ())
    && Cil.hasAttribute Cil.frama_c_inlined b.battrs then begin
      Stack.push true is_inlined_block;
      Cil.DoChildrenPost (fun b' ->
          ignore(Stack.pop is_inlined_block);
          b'
        )
    end
    else
      Cil.DoChildren

  (* Handles lannotate macro and generates mutations and labels as such :
     - START, handles all if statements between START and END
     - END, ends "annotating" zone
     - TARGET, generates labels
     - DOUBLE IF, remember for the next instrumented if form
     - If, mutate if expression
  *)
  method! vstmt_aux stmt =
    match stmt.skind with
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, [], _))
      when String.equal v.vname start ->
      if Stack.is_empty is_inlined_block then Stack.push [] seen_vinfos;
      stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
      Cil.SkipChildren
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, [], _))
      when String.equal v.vname end_crit ->
      if self#in_crit_zone () then begin
        ignore(Stack.pop seen_vinfos);
        seen_double <- false;
        seen_ignore <- false;
      end;
      stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
      Cil.SkipChildren
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, [], loc))
      when String.equal v.vname target ->
      if self#in_crit_zone () then begin
        let top = Stack.pop seen_vinfos in
        if top <> [] then begin
          let label = mk_label (self#generate_disj loc top) [] loc in
          label.labels <- stmt.labels;
          Stack.push top seen_vinfos;
          Cil.ChangeTo label
        end
        else begin
          Options.warning "Success point reached without preceding if at %a, \
                           annotation ignored." Printer.pp_location loc;
          stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
          Cil.SkipChildren
        end
      end
      else begin
        stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
        Cil.SkipChildren
      end
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, [], _))
      when String.equal v.vname double_if ->
      if self#in_crit_zone () then seen_double <- true;
      stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
      Cil.SkipChildren
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, [], _))
      when String.equal v.vname ignore_if ->
      if self#in_crit_zone () then seen_ignore <- true;
      stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
      Cil.SkipChildren
    | If _ when self#in_crit_zone() && seen_ignore ->
      seen_ignore <- false;
      Cil.DoChildren
    | If (exp,thenb,elseb,loc) when self#in_crit_zone() ->
      let mutate_calls, new_exp = self#generate_if_exp exp in
      seen_double <- false;
      let thenb' = Cil.visitCilBlock (self :> Cil.cilVisitor) thenb in
      let elseb' = Cil.visitCilBlock (self :> Cil.cilVisitor) elseb in
      let new_if = Stmt_builder.mk (If (new_exp,thenb',elseb',loc)) in
      stmt.skind <- Block (Cil.mkBlock (mutate_calls @ [new_if]));
      Cil.SkipChildren
    | _ -> Cil.DoChildren

  method! vfile _ =
    Cil.DoChildrenPost (fun f ->
        let clean_globals =
          Cil.foldGlobals f (fun acc g ->
              if self#is_lannotate_builtin g then acc else g :: acc
            ) [] in
        f.globals <- List.rev clean_globals;
        f
      )
end

module RedundantCheckCountermeasures = Annotators.Register (struct
    let name = "RCC"
    let help = "Redundant Check Countermeasures Coverage"
    let apply mk_label file =
      Visitor.visitFramacFileSameGlobals
        (new visitor mk_label :> Visitor.frama_c_visitor) file
  end)