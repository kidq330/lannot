open Cil_types

module IntSet = Set.Make(struct type t = int let compare = compare end)

type non_bottom = {
  complete: IntSet.t;      (* The list of sequences that we completely saw. *)
  seen_first: IntSet.t;    (* The list of sequences for which we saw the first element,
                                   and did not encounter a condition false inbetween. *)
}

type t =
  | Bottom
  | NonBottom of non_bottom


module P() = struct

  (* Mapping from sequence to conditions. *)
  let seq_cond_hash: (int,string) Hashtbl.t = Hashtbl.create 117

  let cond_seq_hash: (string,int) Hashtbl.t = Hashtbl.create 117

  let add_seq_cond_mapping seq cond =
    if not @@ Hashtbl.mem seq_cond_hash seq then
      (Hashtbl.add seq_cond_hash seq cond;
       Hashtbl.add cond_seq_hash cond seq)

  (* Toutes les sequences qui ont ete vues. *)
  let seq_seen= ref IntSet.empty

  (* Toutes les sequences pour lesquelles il y a un chemin qui termine. *)
  let seq_complete = ref IntSet.empty

  type nonrec t = t
  let join a b =
    match a,b with
    | Bottom, x | x, Bottom -> x
    | NonBottom a, NonBottom b ->
      NonBottom{ complete = IntSet.union a.complete b.complete;
                 seen_first = IntSet.union a.seen_first b.seen_first;
               }

  let is_included a b =
    match a,b with
    | Bottom, _ -> true
    | NonBottom _, Bottom -> false
    | NonBottom a, NonBottom b ->
      IntSet.subset a.complete b.complete
      && IntSet.subset a.seen_first b.seen_first

  let pretty _ _ = assert false
  let join_and_is_included a b =
    join a b, is_included a b

  let bottom = Bottom


  let the = function None -> assert false | Some x -> x

  let do_pc_label_sequence args t =
    match args with
    | [_;seq_id;seq_num;total_num;condition;_] -> begin
        let seq_id = Integer.to_int @@ the @@ Cil.isInteger seq_id in
        let seq_num = Integer.to_int @@ the @@ Cil.isInteger seq_num in
        let total_num = Integer.to_int @@ the @@ Cil.isInteger total_num in
        let condition = match condition.enode with
          | Cil_types.(Const(CStr c)) -> c
          | _ -> assert false
        in
        if total_num = 2 then begin
          add_seq_cond_mapping seq_id condition;
          match seq_num with
          | 1 ->
            seq_seen := IntSet.add seq_id !seq_seen;
            { seen_first = IntSet.add seq_id t.seen_first; complete = t.complete }
          | 2 ->
            if IntSet.mem seq_id t.seen_first
            then begin
              seq_complete := IntSet.add seq_id !seq_complete;
              { complete = IntSet.add seq_id t.complete; seen_first = t.seen_first }
            end
            else t
          | _ -> assert false
        end else t
      end
    | _ -> assert false

  let do_pc_label_sequence_condition args t =
    match args with
    | [_;cond_id] -> begin
        let cond_id = match cond_id.enode with
          | Cil_types.(Const(CStr c)) -> c
          | _ -> assert false
        in
        (* Remove seen_first for all sequences matching the condition. *)
        let sequences = Hashtbl.find_all cond_seq_hash cond_id in
        let remove_seen_first = List.fold_left (fun acc seq ->
            IntSet.remove seq acc) t.seen_first sequences in
        { seen_first = remove_seen_first; complete = t.complete}


      end
    | _ -> assert false

  let do_call func args t =
    match t with
    | Bottom -> Bottom
    | NonBottom t ->
      let res =
        match func.enode with
        | Lval(Var vi,NoOffset) when vi.vname = "pc_label_sequence" -> do_pc_label_sequence args t
        | Lval(Var vi,NoOffset) when vi.vname = "pc_label_sequence_condition" ->
          do_pc_label_sequence_condition args t
        | _ -> t
      in NonBottom res


  let transfer_stmt stmt t =
    (* Self.feedback "Stmt %a" Cil_datatype.Stmt.pretty stmt; *)
    match stmt.skind with
    | Instr(Call(None,func,args,_)) ->
      let res = do_call func args t in
      List.map (fun x -> (x,res)) stmt.succs
    | _ -> List.map (fun x -> (x,t)) stmt.succs

end



let do_function kf =
  if Kernel_function.is_definition kf then begin
    let module Fenv = (val Dataflows.function_env kf) in
    let module Inst = P() in
    let module Arg = struct
      include Inst
      let init =
        let initial_value =
          NonBottom{complete = IntSet.empty;
                    seen_first = IntSet.empty}
        in
        [(Kernel_function.find_first_stmt kf, initial_value)]

    end in
    let module Analysis = Dataflows.Simple_forward(Fenv)(Arg) in
    IntSet.elements @@ IntSet.diff !Inst.seq_seen !Inst.seq_complete
  end else []

class remove_stmts data tbl = object(self)
  inherit Visitor.frama_c_inplace

  val mutable to_remove = IntSet.empty
  val mutable fun_stmts : int list = []

  method private clean dec =
    to_remove <- IntSet.empty;
    let infeasible_list = do_function (Extlib.the self#current_kf) in
    if infeasible_list <> [] then begin
      Options.feedback "Cleaning...";
      Printf.printf "    Number of sequences found infeasible in %s : %d\n" dec.svar.vname (List.length infeasible_list)
    end;
    data := List.filter (fun (_,_,ids) -> not (List.exists (fun id -> id = ids) infeasible_list)) !data;
    fun_stmts <- Utils.concat @@ List.map (fun sid -> Hashtbl.find_all tbl sid) infeasible_list ;
    dec.sbody <- (Cil.visitCilBlock (self :> Cil.cilVisitor) dec.sbody)

  method! vfunc (dec : Cil_types.fundec) : Cil_types.fundec Cil.visitAction =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      self#clean dec;
      Cil.SkipChildren
    end

  method! vblock _ =
    Cil.DoChildrenPost (fun block ->
        let rec aux acc = function
          | [] -> acc
          | s :: t ->
            if List.exists (fun s' -> s' = s.sid) fun_stmts then
              aux acc t
            else aux (s::acc) t
        in block.bstmts <- List.rev (aux [] block.bstmts);
        block
      )

    method! vstmt s =
    match s.skind with
    | UnspecifiedSequence v ->
      s.skind <- Block (Cil.block_from_unspecified_sequence v); Cil.DoChildren
    | _ -> Cil.DoChildren

end

let clean data tbl file =
  Visitor.visitFramacFileSameGlobals (new remove_stmts data tbl :> Visitor.frama_c_visitor) file
