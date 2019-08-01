open Cil_types
open Ast_const

type def = {
  funId: int;    (* (function id) if this def is from a function's formals, -1 otherwise *)
  varId: int;    (* id of the variable being defined *)
  defId: int;    (* current definition of the variable (i.e. (number of definition before) + 1 *)
  stmtDef: int;  (* Statement id, will be use to "attach" the sequence to this statement at the end *)
}

module DefSet = Set.Make(struct type t = def let compare = compare end)

type t =
  | Bottom
  | NonBottom of DefSet.t

(* bind each new sequence (def) to its corresponding statement *)
let to_add_defs : (int, int*stmt) Hashtbl.t = Hashtbl.create 32
(* bind each new sequence (use)  to its corresponding statement *)
let to_add_uses : (int, int*stmt) Hashtbl.t = Hashtbl.create 32
(* bind each new sequence (cond) to its corresponding statement *)
let to_add_cond : (int, stmt) Hashtbl.t = Hashtbl.create 32
(* bind each new sequence (def) to its corresponding function (for function formals) *)
let to_add_fun : (int, int*stmt) Hashtbl.t = Hashtbl.create 32

(* For each variable, keep the number of assignment seen for this variable *)
let def_id : (int, int) Hashtbl.t = Hashtbl.create 32

(* bind a variable id and an index to a new id  *)
let index_vid : (int*int,int) Hashtbl.t = Hashtbl.create 32
(* Used to find all indexes seen for a specific vid  *)
let vid_to_index : (int,int) Hashtbl.t = Hashtbl.create 32

(* since cil expr have no side effect, we do not need to create more than one sequence if
 a lval is used more than once in an expr *)
let done_expr : (int*int,DefSet.t) Hashtbl.t = Hashtbl.create 32

(* each def (statement id) are registered to avoid creating them twice (or more) with a new id
   (for example, in loop, each statement will be computed at least 2 times to find a fixpoint) *)
let seen_def : (int,def) Hashtbl.t = Hashtbl.create 32

(* Used to create hyperlabels. key = (variable id, definition id), value = sequence id *)
let hyperlabels : (int*int, int list) Hashtbl.t = Hashtbl.create 32

(* count the number of sequences *)
let nb_seqs = ref 0

(* Clear all hashtbl after each function in addSequences *)
let reset_all () : unit =
  Hashtbl.reset to_add_defs;
  Hashtbl.reset to_add_uses;
  Hashtbl.reset to_add_cond;
  Hashtbl.reset to_add_fun;
  Hashtbl.reset def_id;
  Hashtbl.reset index_vid;
  Hashtbl.reset done_expr;
  Hashtbl.reset seen_def

(* Given a variable id, increment the number of defs seen, and returns it *)
let get_next_def_id (vid:int) : int  =
  if Hashtbl.mem def_id vid then begin
    let n = (Hashtbl.find def_id vid) + 1 in
    Hashtbl.replace def_id vid n;
    n
  end
  else
    (Hashtbl.add def_id vid 1; 1)

(* Given a variable id and its index, create/return the corresponding new id *)
let get_index_vid (vid:int) (index:int) : int =
  if Hashtbl.mem index_vid (vid,index) then
    Hashtbl.find index_vid (vid,index)
  else begin
    let new_vid =  Cil_const.new_raw_id () in
    Hashtbl.add vid_to_index vid index;
    Hashtbl.add index_vid (vid,index) new_vid;
    new_vid
  end

(* Create a new def (not a function's formals) *)
let make_def ?(funId = (-1)) (varId: int) (defId: int) (stmtDef: int) : def =
  {funId;varId;defId;stmtDef}

(* for a given variable id, returns all indexes seen so far for this id *)
let get_all_index_vid (vid:int) : int list =
  let all_index = Hashtbl.find_all vid_to_index vid in
  List.map (fun i -> Hashtbl.find index_vid (vid,i)) all_index

(* If 'index' can be reduced to a constant, then create a new id, associate it to the pair
   (variable id, 'index') and returns it, else return vid.
   If we can't recuce index, then return all seen indexes plus the current vid *)
let extract_index (vid:int) (index:Cil_types.offset) : int list =
  if not (Options.FoldIndex.get ()) then [vid]
  else
    match index with
    | Index(index,NoOffset) ->
      begin match Cil.constFoldToInt index with
        | None ->
          vid :: get_all_index_vid vid
        | Some i ->
          let i = Integer.to_int i in
          [get_index_vid vid i]
      end
    | _ ->
      [vid]

(* Add a sequence id to its corresponding hyperlabel *)
let add_to_hyperlabel (key:int*int) (ids:int) : unit =
  if Hashtbl.mem hyperlabels key then
    let old = Hashtbl.find hyperlabels key in
    Hashtbl.replace hyperlabels key (ids::old)
  else
    Hashtbl.add hyperlabels key [ids]

(* Given a pair (stmt id, expr id), returns all defs already done for this expr *)
let get_done_set (key:int*int) : DefSet.t =
  if Hashtbl.mem done_expr key then
    Hashtbl.find done_expr key
  else
    DefSet.empty

(* Add the given def to the set of done defs for the corresponding key *)
let set_done_set (key:int*int) (def:def) : unit =
  if Hashtbl.mem done_expr key then
    let old = Hashtbl.find done_expr key in
    Hashtbl.replace done_expr key (DefSet.add def old)
  else
    Hashtbl.add done_expr key (DefSet.singleton def)

(* This visitor visits each LVal to create sequences if the state t contains defs for this LVal *)
class visit_exp (t:t) (sid:int) = object(self)
  inherit Visitor.frama_c_inplace

  (* Since expr (and inside of instr) don't have any side effect,
     we can create only one sequence per lval inside it,
     even if they are used more than once*)
  val mutable visited : int list = []

  (* Test if a LVal shoudl be instrumented, i.e. it's not a global, a temp variable
     or if it's the first time we saw it in the current expr/instr
     (except if CleanExp is true) *)
  method private should_instrument v vid =
    not v.vglob
    && not (v.vname = "__retres")
    && not v.vtemp
    && (not (Options.CleanDuplicate.get())
        || not (List.exists (fun vid' -> vid' = vid) visited))

  (* Create a condition which will break sequences for this variable *)
  method private mkCond (vid: int) : Cil_types.stmt =
    let zeroExp = Exp.zero () in
    let ccExp = Exp.string (string_of_int vid) in
    Utils.mk_call "pc_label_sequence_condition" ([zeroExp;ccExp])

  (* Create a sequence member (def if is_def is true, use otherwise) *)
  method private mkSeq_aux (ids: int) (vid: int) (is_def:bool) : Cil_types.stmt =
    let idExp = Exp.kinteger IULong ids in
    let oneExp = Exp.one () in
    let curr = if is_def then Exp.integer 1 else Exp.integer 2 in
    let slen = Exp.integer 2 in
    let varExp = Exp.string (string_of_int vid) in
    let zeroExp = Exp.zero () in
    Utils.mk_call "pc_label_sequence" ([oneExp;idExp;curr;slen;varExp;zeroExp])

  (* Create for a given def the sequence def-use for the current LVal *)
  method private mkSeq eid def =
    let ids = Annotators.next () in
    let sdef,suse = self#mkSeq_aux ids def.varId true, self#mkSeq_aux ids def.varId false in
    if def.funId = -1 then begin
      if not (Hashtbl.mem to_add_cond def.stmtDef) then
        Hashtbl.add to_add_cond def.stmtDef (self#mkCond def.varId);
      Hashtbl.add to_add_defs def.stmtDef (ids,sdef)
    end
    else
      Hashtbl.add to_add_fun def.funId (ids,sdef);
    Hashtbl.add to_add_uses sid (ids,suse);
    add_to_hyperlabel (def.varId, def.defId) ids;
    set_done_set (sid,eid) def;
    incr nb_seqs

  (* Get all the definition of a Lval  *)
  method! vexpr expr =
    match t with
    | Bottom -> Cil.SkipChildren
    | NonBottom t ->
      begin match expr.enode with
        | Lval (Var v, index) ->
          let vids = extract_index v.vid index in
          let f vid =
            if self#should_instrument v vid then begin
              visited <- vid :: visited;
              let already_done = get_done_set (sid, expr.eid) in
              let all_vid_defs = DefSet.filter (fun def -> def.varId = vid) t in
              let all_vid_defs_todo = DefSet.diff all_vid_defs already_done in
              DefSet.iter (self#mkSeq expr.eid) all_vid_defs_todo;
            end
          in
          List.iter f vids;
          Cil.DoChildren
        | _ -> Cil.DoChildren
      end
end

module P() = struct

  let print_elt elt =
    Printf.printf "funId: %d / varId: %d / defId: %d / stmtDef: %d\n%!"
      elt.funId elt.varId elt.defId elt.stmtDef

  let print_defs set =
    DefSet.iter (fun elt -> Printf.printf "    %!"; print_elt elt) set

  let pretty _ = function
    | Bottom -> Printf.printf "State: Bottom\n%!"
    | NonBottom t ->
      Printf.printf "NonBottom : \n%!";
      print_defs t

  (* Return a new set after removing all definitions of a variable *)
  let remove_def vid s =
    DefSet.filter (fun v -> v.varId <> vid) s

  type nonrec t = t

  (* Function called to join 2 states *)
  let join a b =
    match a,b with
    | Bottom, x | x, Bottom -> x
    | NonBottom a, NonBottom b ->
      NonBottom (DefSet.union a b)

  (* is the set a is a subset of b*)
  let is_included a b =
    match a,b with
    | Bottom, _ -> true
    | NonBottom _, Bottom -> false
    | NonBottom a, NonBottom b ->
      DefSet.subset a b

  let join_and_is_included a b =
    join a b, is_included a b

  let bottom = Bottom

  let the = function None -> assert false | Some x -> x

  (* For each definition statement, change the current state by adding or not new definition,
  removing older ones etc... *)
  let do_def ?(index=NoOffset) v sid = function
    | Bottom -> Bottom
    | NonBottom t ->
      let vids = extract_index v.vid index in
      let f acc vid =
        let removed_vid =
          if Options.CleanDataflow.get () then
            remove_def vid acc
          else
            acc
        in
        if Hashtbl.mem seen_def sid then
          DefSet.add (Hashtbl.find seen_def sid) removed_vid
        else begin
          let defId = get_next_def_id vid in
          let new_def = make_def vid defId sid in
          Hashtbl.add seen_def sid new_def;
          DefSet.add new_def removed_vid
        end
      in
      NonBottom (List.fold_left f t vids)

  (* Function called for each stmt and propagating new states to each succs of stmt *)
  let transfer_stmt stmt t =
    match stmt.skind with
    | Instr i when not (Utils.is_label i) ->
      ignore(Cil.visitCilInstr (new visit_exp t stmt.sid :> Cil.cilVisitor) i);
      begin match i with
        | Set ((Var v,index),_,_)
        | Call (Some (Var v,index),_,_,_) ->
          if not (v.vname = "__retres") && not v.vtemp then begin
            let res = do_def ~index v stmt.sid t in
            List.map (fun x -> (x,res)) stmt.succs
          end
          else List.map (fun x -> (x,t)) stmt.succs
        | Local_init (v,_,_) ->
          if not (v.vname = "__retres") && not v.vtemp then begin
            let res = do_def v stmt.sid t in
            List.map (fun x -> (x,res)) stmt.succs
          end
          else List.map (fun x -> (x,t)) stmt.succs
        | _ -> List.map (fun x -> (x,t)) stmt.succs
      end
    | Return (Some e,_)
    | If (e,_,_,_)
    | Switch (e,_,_,_) ->
      ignore(Cil.visitCilExpr (new visit_exp t stmt.sid :> Cil.cilVisitor) e);
      List.map (fun x -> (x,t)) stmt.succs
    | _ -> List.map (fun x -> (x,t)) stmt.succs

end

(* For each function, do a dataflow analysis and create sequences
   Initial state contains def of each formal
*)
let do_function kf =
  let module Fenv = (val Dataflows.function_env kf) in
  let module Inst = P() in
  let args = Kernel_function.get_formals kf in
  let first_stmt = Kernel_function.find_first_stmt kf in
  let init = ref DefSet.empty in
  let f arg =
    let defId = get_next_def_id arg.vid in
    init := DefSet.add (make_def ~funId:(Kernel_function.get_id kf) arg.vid defId first_stmt.sid) !init
  in
  List.iter f args;
  let module Arg = struct
    include Inst
    let init =
      [(first_stmt, NonBottom (!init))]

  end in
  let module Analysis = Dataflows.Simple_forward(Fenv)(Arg) in
  ()

(* This visitor will, for each function :
   - Do the dataflows analysis and fill hashtbls
   - Then visit the body of the function, add add all sequences to where
   they belong.
   - Clean all hashtbls and do the next function (if any)
*)
class addSequences= object(self)
  inherit Visitor.frama_c_inplace

  (* get all sequences, sort defs and uses by sequence ID, and returns
  a pair of list (before,after) with sequences to add before & after the current statement *)
  method private get_seqs_sorted sid =
    let defs = Hashtbl.find_all to_add_defs sid in
    let uses = Hashtbl.find_all to_add_uses sid in
    let cond = Hashtbl.find_all to_add_cond sid in
    let compare (id1,_) (id2,_) = compare id1 id2 in
    let defs,uses = List.sort compare defs, List.sort compare uses in
    List.map (fun (_,s) -> s) defs, (List.map (fun (_,s) -> s) uses) @ cond

  method! vfunc (dec : Cil_types.fundec) : Cil_types.fundec Cil.visitAction =
    let kf = Extlib.the self#current_kf in
    if Kernel_function.is_definition kf && Annotators.shouldInstrument dec.svar then begin
      Cfg.clearCFGinfo ~clear_id:false dec;
      Cfg.cfgFun dec;
      do_function kf;
      Cil.DoChildrenPost (fun f ->
          let id = dec.svar.vid in
          let defs = List.sort compare (Hashtbl.find_all to_add_fun id) in
          let params = List.map (fun (_,s) -> s) defs in
          f.sbody.bstmts <- params @ f.sbody.bstmts;
          reset_all ();
          f
        )
    end
    else Cil.SkipChildren

  method! vblock _ =
    Cil.DoChildrenPost (fun block ->
        let rec aux l acc =
          match l with
          | [] -> acc
          | s :: t ->
            let after,before = self#get_seqs_sorted s.sid in
            (* if the statement has 1 or more labels, then moves it to
               the first statement of before if it exists *)

            if s.labels <> [] && before <> [] then begin
              s.skind <- Block (Block.mk (before @ [Stmt.mk s.skind]));
              aux t (acc @ [s] @ after)
            end
            else
              aux t (acc @ before @ [s] @ after)
        in block.bstmts <- aux block.bstmts [];
        block
      )

  method! vstmt s =
    match s.skind with
    | UnspecifiedSequence v ->
      s.skind <- Block (Cil.block_from_unspecified_sequence v); Cil.DoChildren
    | _ -> Cil.DoChildren

end

(** Hyperlabel's type *)
let symb : string ref = ref ""

(** Create all hyperlabels *)
let compute_hl () : string =
  if "-" = !symb then
    Hashtbl.fold (fun _ seqs str ->
        let seqs = List.sort compare seqs in
        List.fold_left (fun acc s -> acc ^ Annotators.next_hl() ^ ") <s" ^ string_of_int s ^"|; ;>,\n") str seqs
      ) hyperlabels ""
  else
    Hashtbl.fold (fun _ seqs str ->
        str ^ Annotators.next_hl() ^ ") <" ^ (String.concat !symb (List.map (fun s -> "s" ^ string_of_int s) seqs)) ^ "|; ;>,\n"
      ) hyperlabels ""

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl () in
  let out = open_out_gen [Open_creat; Open_append] 0o644 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of sequences = %d" !nb_seqs;
  Options.feedback "Total number of hyperlabels = %d" (Annotators.getCurrentHLId())

(** Successively pass the 2 visitors *)
let visite (file : Cil_types.file) : unit =
  Visitor.visitFramacFileSameGlobals (new addSequences :> Visitor.frama_c_visitor) file;
  Cfg.clearFileCFG ~clear_id:false file;
  Cfg.computeFileCFG file;
  Ast.mark_as_changed ();

  (** All-defs annotator *)
module ADC = Annotators.Register (struct
    let name = "ADC"
    let help = "All-Definitions Coverage"
    let apply _ file =
      visite file;
      symb := "+";
      gen_hyperlabels ()
  end)


(** All-uses annotator *)
module AUC = Annotators.Register (struct
    let name = "AUC"
    let help = "All-Uses Coverage"
    let apply _ file =
      visite file;
      symb := ".";
      gen_hyperlabels ()
  end)

(** Def-Use annotator *)
module DUC = Annotators.Register (struct
    let name = "DUC"
    let help = "Definition-Use Coverage"
    let apply _ file =
      visite file;
      symb := "-";
      gen_hyperlabels ()
  end)
