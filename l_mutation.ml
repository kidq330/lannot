(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2020                                               *)
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
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Ast_const
open Utils

(** RCC Visitor **)
class visitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  (* Used to remember when inline code start *)
  val started_inline = Stack.create ()
  (* List of last temporary variables used to store mutations *)
  val mutable seen_vinfos : varinfo list= []
  (* When started is true, mutate all if until success point is found *)
  val mutable started : bool = false
  (* Remember if a macro duble was seen for the next if *)
  val mutable seen_double : bool = false
  (* Id used to create temporary variable names *)
  val mutable id : int = 0

  (* Get the next id to use *)
  method private next () : int =
    id <- id + 1; id

  (* Returns true if :
     - we're not inside an inlined code
     - the code correspond to the original inlined function
       returns false otherwise
  *)
  method private should_instrument () : bool =
    let kf = Extlib.the self#current_kf in
    let fdec = Kernel_function.get_definition kf in
    match Stack.top_opt started_inline with
    | None -> true
    | Some fun_name' ->
      String.equal fdec.svar.vorig_name fun_name'

  (* Creates a new temporary variable and adds it to seen_vinfos *)
  method private get_new_tmp_var () : varinfo =
    let kf = Extlib.the self#current_kf in
    let fdec = Kernel_function.get_definition kf in
    let name = "lannot_mut_"^string_of_int (self#next()) in
    let vi = Cil.makeTempVar ~name fdec (TInt(IInt,[])) in
    seen_vinfos <- vi :: seen_vinfos;
    vi

  (* For a given if expression, creates the corresponding mutated expression
     of the form (mut_exp && !e || !mut_exp && e), and assigns the temporary
     variable with mutate function (cf. Printer in utils.ml)
  *)
  method private generate_exp (exp: exp) : (stmt*exp) =
    let mutate_lval = Cil.var (self#get_new_tmp_var ()) in
    let loc = exp.eloc in
    let mutate_exp = Cil.new_exp ~loc (Lval mutate_lval) in
    let mutation_side = Cil.mkBinOp ~loc LAnd mutate_exp (Exp.lnot exp) in
    let no_mutation_side = Cil.mkBinOp ~loc LAnd (Exp.lnot mutate_exp) exp in
    let mutate_call = Utils.mk_call ~result:mutate_lval "mutate" [] in
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
        Options.warning "If at %a wrongly marked as double.\n\
                         \t Parsing can split if statement if it contains side effects.\
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
        aux tl (Some(Cil.new_exp ~loc (Lval (Cil.var vInfo))))
      | vInfo :: tl, Some acc ->
        let new_lval = (Cil.new_exp ~loc (Lval (Cil.var vInfo))) in
        aux tl (Some(Cil.mkBinOp ~loc LOr new_lval acc))
      | _ -> assert false
    in
    aux vinfos None

  (* Clears all parameters after each function *)
  method! vfunc _ =
    Cil.DoChildrenPost( fun fdec ->
        Stack.clear started_inline;
        seen_vinfos <- [];
        started <- false;
        seen_double <- false;
        fdec
      )

  (* Handles lannotate macro and generates mutations and labels as such :
     - START/END INLINE, used to know where we are
       (inlined code/inline function/normal code)
     - START, handles all if statements between START and SUCCESS
       (except for inlined code, cf. should_instrument)
     - SUCCESS, ends "annotating" zone, and generates labels
     - DOUBLE IF, remember for the next instrumented if form
     - If, mutate if expression
  *)
  method! vstmt_aux stmt =
    match stmt.skind with
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, fun_name :: [], _)) when String.equal v.vname start_inline ->
      let fun_name =  Extlib.the (is_cil_string fun_name) in
      Stack.push fun_name started_inline;
      stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
      Cil.SkipChildren
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, fun_name :: [], _)) when String.equal v.vname end_inline ->
      let fun_name =  Extlib.the (is_cil_string fun_name) in
      stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
      (match Stack.pop_opt started_inline with
       | Some fun_name' when (String.equal fun_name' fun_name) ->
         Cil.SkipChildren
       | _ -> assert false)
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, [], _)) when String.equal v.vname start ->
      if self#should_instrument () then started <- true;
      stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
      Cil.SkipChildren
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, step :: [], loc)) when String.equal v.vname target ->
      let step = Integer.to_int (Extlib.the (Cil.isInteger step)) in
      if started && self#should_instrument () then
        if seen_vinfos <> [] then begin
          let label = mk_label (self#generate_disj loc seen_vinfos) [] loc in
          label.labels <- stmt.labels;
          if step = 0 then (seen_vinfos <- []; started <- false);
          Cil.ChangeTo label
        end
        else begin
          Options.warning "Success point reached without preceding if at %a, annotation ignored." Printer.pp_location loc;
          stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
          Cil.SkipChildren
        end
      else begin
        stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
        Cil.SkipChildren
      end
    | Instr (Call (_, {enode=Lval (Var v, NoOffset)}, [], _)) when String.equal v.vname double_if ->
      stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
      if started && self#should_instrument () then
        seen_double <- true;
      Cil.SkipChildren
    | If (exp,thenb,elseb,loc) when started && self#should_instrument () ->
      let mutate_calls, new_exp = self#generate_if_exp exp in
      seen_double <- false;
      let thenb' = (Cil.visitCilBlock (self :> Cil.cilVisitor) thenb) in
      let elseb' = (Cil.visitCilBlock (self :> Cil.cilVisitor) elseb) in
      let new_if = Stmt.mk (If (new_exp,thenb',elseb',loc)) in
      stmt.skind <- Block (Block.mk (mutate_calls @ [new_if]));
      Cil.SkipChildren
    | _ -> Cil.DoChildren

  method! vfile _ =
    Cil.DoChildrenPost (fun f ->
        let clean_globals =
          Cil.foldGlobals f (fun acc g ->
              if Utils.is_lannotate_builtin g then acc else g :: acc
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