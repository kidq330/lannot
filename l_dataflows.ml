open Cil_types
open Ast_const

(* Main type used for dataflow analysis *)
type def = {
  (* Unique ID for each def, in increasing order. *)
  id: int;
  (* (function id) if this def is from a function's formals, -1 otherwise. *)
  funId: int;
  (* id of the variable being defined. *)
  varId: int;
  (* Current definition of the variable (number of def seen before + 1). *)
  defId: int;
  (* Statement id, will be use to "add" the sequence to its stmt at the end. *)
  stmtDef: int;
}

(* Main type used for our analysis's state *)
module DefSet = Set.Make (
  struct type t = def
    let compare d1 d2 = compare d1.id d2.id
  end)

(* Represent states in our analysis*)
type t =
  | Bottom
  | NonBottom of DefSet.t

(* bind each new sequence (def) to its corresponding statement, sequence id is
   used to sort them at the end
   key : statement id
   value : (sequence id * statement) list
*)
let to_add_defs : (int, (int*stmt) list) Hashtbl.t = Hashtbl.create 32
(* bind each new sequence (use)  to its corresponding statement *)
let to_add_uses : (int, (int*stmt) list) Hashtbl.t = Hashtbl.create 32
(* bind each new sequence (cond) to its corresponding statement *)
let to_add_cond : (int, stmt) Hashtbl.t = Hashtbl.create 32
(* bind each new sequence (def) to its corresponding function (for formals)
   key : function id
   value : (sequence id * statement) list
*)
let to_add_fun : (int, (int*stmt) list) Hashtbl.t = Hashtbl.create 32

(* For each variable, keep the number of assignment seen for this variable
   key : variable id
   value : def counter
*)
let def_id : (int, int) Hashtbl.t = Hashtbl.create 32

(* Used to assign unique id for specific variable and field of structs
   key : variable id * field name
   value : unique id
*)
let offset_id : (int*string, int) Hashtbl.t = Hashtbl.create 32

(* remember each def already processed to avoid doing them again
   key : statement id * expression id
   value : set of def done for this expression
*)
let done_set : (int*int,DefSet.t) Hashtbl.t = Hashtbl.create 32

(* each def is registered to avoid creating them more than once
   key : statement id
   value : def
*)
let seen_def : (int,def) Hashtbl.t = Hashtbl.create 32

(* WIP: for each couple vid (variable used) and block, save the state...
   key : variable id * block
   value : state at this point
*)
let equivalent_tbl : (int*block,DefSet.t) Hashtbl.t = Hashtbl.create 32

(* Used to create hyperlabels
   key : variable id * definition id
   value : (sequence id) list
*)
let hyperlabels : (int*int, int list) Hashtbl.t = Hashtbl.create 32

(* Count the number of sequences. *)
let nb_seqs = ref 0

(* Def's ID, used to know the order of seen def during dataflow analysis. *)
let count_def = ref 1
let next () =
  let tmp = !count_def in
  incr count_def;
  tmp

(* Used for context criteria, count the number of labels avoided due to MaxContextPath option*)
let ignoredLabels = ref 0

(* Clear all hashtbl and reset counter after each function in addSequences *)
let reset_all () : unit =
  Hashtbl.reset to_add_defs;
  Hashtbl.reset to_add_uses;
  Hashtbl.reset to_add_cond;
  Hashtbl.reset to_add_fun;
  Hashtbl.reset def_id;
  Hashtbl.reset offset_id;
  Hashtbl.reset equivalent_tbl;
  Hashtbl.reset done_set;
  Hashtbl.reset seen_def;
  count_def := 0

(* generic function for Hashtbl which use list as values.
   If the binding key exists, adds elt to its list,
   else create a new binding. *)
let replace_or_add_list (tbl:('a,'b list) Hashtbl.t) (key:'a) (elt:'b) =
  if Hashtbl.mem tbl key then begin
    let old = Hashtbl.find tbl key in
    Hashtbl.replace tbl key (elt::old)
  end
  else
    Hashtbl.add tbl key [elt]

(* Given a variable id, increment the number of defs seen
   and returns it. If it is the first return 1. *)
let get_next_def_id (vid:int) : int  =
  if Hashtbl.mem def_id vid then begin
    let n = (Hashtbl.find def_id vid) + 1 in
    Hashtbl.replace def_id vid n;
    n
  end
  else
    (Hashtbl.add def_id vid 1; 1)

(* Create a new def. *)
let make_def ?(funId = (-1)) (varId: int) (defId: int) (stmtDef: int) : def =
  {id=next();funId;varId;defId;stmtDef}

(* Add a sequence id to its corresponding hyperlabel *)
let add_to_hyperlabel (key:int*int) (ids:int) : unit =
  replace_or_add_list hyperlabels key ids

(* Returns all defs already done for this expression and statement
   Returns empty set if it does not exist *)
let get_done_set (key:int*int) : DefSet.t =
  if Hashtbl.mem done_set key then
    Hashtbl.find done_set key
  else
    DefSet.empty

(* Given a pair (stmt id, expr id), returns all defs already done for this expr *)
let get_all_done_set (key:int*int) : DefSet.t list =
  Hashtbl.find_all done_set key

(* Add the given def to the set of done_def for the corresponding key *)
let add_to_done_set (key:int*int) (def:def) : unit =
  if Hashtbl.mem done_set key then
    let old = Hashtbl.find done_set key in
    Hashtbl.replace done_set key (DefSet.add def old)
  else
    Hashtbl.add done_set key (DefSet.singleton def)

let add_done_set (key:int*int) (set:DefSet.t) : unit =
  Hashtbl.add done_set key set

(* For a given vid and field name, returns their unique id *)
let get_offset_id (vid:int) (field_name:string) : int =
  if Hashtbl.mem offset_id (vid,field_name) then
    Hashtbl.find offset_id (vid,field_name)
  else begin
    let new_id = Cil_const.new_raw_id () in
    Hashtbl.add offset_id (vid,field_name) new_id;
    new_id
  end

(* Recursively create new unique id for each field used with the variable.
   Exemple :
   A_struct.field1.field2id will be :
   create new_vid  for (A_struct.vid, field1)
   create new_vid2 for (new_vid     ,field2)
   returns new_vid2
*)
let rec get_vid_with_field (vid:int) (offset:offset) : int =
  if Options.HandleStruct.get () then
    match offset with
    | Field (f_info,offset') ->
      let new_id = get_offset_id vid f_info.fname in
      get_vid_with_field new_id offset'
    | _ -> vid
  else
    vid

(** Return the cartesian product between all lists of def given in parameters
    [[a;b];[c;d]] -> [[a;c];[a;d];[b;c];[b;d]] ...
*)
let rec n_cartesian_product (ll : def list list) : def list list =
  match ll with
  | [] -> assert false
  | [l] -> List.fold_left (fun acc i -> [i]::acc) [] l
  | h :: t ->
    let rest = n_cartesian_product t in
    List.concat
      (List.fold_left (fun acc i -> acc@[List.fold_left (fun acc2 r -> (i :: r)::acc2) [] rest]) [] h)

(* Given a list of def list (For each variable, each definitions), returns all possible combinations
of those definitions that were not already computed before *)
let make_combs expr sid (combs: def list list) : DefSet.t list =
  let prod_size = List.fold_left (fun acc l -> acc * (List.length l)) 1 combs in
  if prod_size <= Options.MaxContextPath.get () then begin
    let all_cases = n_cartesian_product combs in
    let all_cases = List.map (fun dl -> DefSet.of_list dl) all_cases in
    let already_done_combs = get_all_done_set (sid, expr.eid) in
    let all_cases = List.filter (fun c1 -> not (List.exists (fun c2 -> DefSet.equal c1 c2) already_done_combs)) all_cases in
    all_cases
  end
  else
    (ignoredLabels := (List.length combs + 1) * prod_size + !ignoredLabels;
     Options.warning "Expression ignored in file %a, too many paths (%d)"
       Printer.pp_location expr.eloc prod_size; [])

(* Create a sequence member
   ids : sequence id
   vid : variable id
   id : position of this member (ex. 1=def and 2=use for def-use pairs
   max : size of the sequence (2 for def-use pairs)
 *)
let  mkSeq_aux (ids: int) (vid: string) (id:int) (max:int) : stmt =
  let idExp = Exp.kinteger IULong ids in
  let oneExp = Exp.one () in
  let curr = Exp.integer id in
  let slen = Exp.integer max in
  let varExp = Exp.string vid in
  let zeroExp = Exp.zero () in
  Utils.mk_call "pc_label_sequence" ([oneExp;idExp;curr;slen;varExp;zeroExp])

(* Create a condition which will break sequences for the variable vid *)
let mkCond (vid: int) : stmt =
  let zeroExp = Exp.zero () in
  let ccExp = Exp.string (string_of_int vid) in
  Utils.mk_call "pc_label_sequence_condition" ([zeroExp;ccExp])

(* Remember variable processed to allow the possibility
   of removing trivially duplicated sequence*)
let visited : int list ref = ref []

(* Returns true if the given variable id has been processed before *)
let is_duplicate_triv (vid:int) : bool =
  List.exists (fun vid' -> vid' = vid) !visited

(* For a given variable id and block, returns true if we have seen
   the same state before *)
let is_equivalent (vid:int) (b:block) (st:DefSet.t) : bool =
  if Hashtbl.mem equivalent_tbl (vid,b) then
    let st' = Hashtbl.find equivalent_tbl (vid,b) in
    DefSet.equal st st'
  else false

(* Test if a LVal should be instrumented, i.e. it's not a global,
   a temp variable or if it's the first time we see it in the current
   expr/instr (except if CleanDuplicate is true) *)
let should_instrument (v:varinfo) (vid:int) : bool =
  not v.vglob && not (v.vname = "__retres") && not v.vtemp &&
  (not (is_duplicate_triv vid) || not (Options.CleanDuplicate.get ()))

let clean_equivalent_tbl all_b =
  let f (vid,b) _ =
    if List.exists (fun b' -> Cil_datatype.Block.equal b b') all_b then
      Hashtbl.remove equivalent_tbl (vid,b)
  in
  Hashtbl.iter f equivalent_tbl

(***********************************)
(********* Defuse Criteria *********)
(***********************************)

(* This visitor visits each LVal to create sequences if the state t contains
   defs for theses LVals *)
class visit_defuse (t:t) (current_stmt:stmt) = object(self)
  inherit Visitor.frama_c_inplace

  val current_block : block = Kernel_function.find_enclosing_block current_stmt
  val sid : int = current_stmt.sid

  (* Create a def-use sequence for the given def *)
  method private mkSeq (eid:int) (def:def) =
    let ids = Annotators.next () in (* sequence id *)
    let sdef = mkSeq_aux ids (string_of_int def.varId) 1 2 in (* def part *)
    let suse = mkSeq_aux ids (string_of_int def.varId) 2 2 in (* use part *)
    (* Add sdef to either defs table or function table *)
    if def.funId = -1 then begin
      (* Add a cond label for this def *)
      if not (Hashtbl.mem to_add_cond def.stmtDef) then
        Hashtbl.add to_add_cond def.stmtDef (mkCond def.varId);
      replace_or_add_list to_add_defs def.stmtDef (ids,sdef)
    end
    else
      replace_or_add_list to_add_fun def.funId (ids,sdef);
    (* Add the use *)
    replace_or_add_list to_add_uses sid (ids,suse);
    (* Register this sequence to its hyperlabel *)
    add_to_hyperlabel (def.varId, def.defId) ids;
    (* Remember this def as done *)
    add_to_done_set (sid,eid) def;
    incr nb_seqs

  (* Visit all expressions and sub expressions to find lvals *)
  method! vexpr expr =
    match t with
    | Bottom -> Cil.SkipChildren
    | NonBottom t ->
      begin match expr.enode with
        | Lval (Var v, offset) ->
          let vid = get_vid_with_field v.vid offset in
          if should_instrument v vid then begin
            visited := vid :: !visited;
            (* Get def already done for this lval *)
            let already_done = get_done_set (sid, expr.eid) in
            (* Get all def in our state which correspond to this variable *)
            let all_vid_defs = DefSet.filter (fun def -> def.varId = vid) t in
            (* Keep only those not already done *)
            let all_vid_defs_todo = DefSet.diff all_vid_defs already_done in
            let is_eq = is_equivalent vid current_block all_vid_defs_todo in
            Hashtbl.replace  equivalent_tbl (vid,current_block) all_vid_defs_todo;
            if not is_eq || not (Options.CleanDuplicate.get ()) then
              DefSet.iter (self#mkSeq expr.eid) all_vid_defs_todo;
          end;
          Cil.DoChildren
        | _ -> Cil.DoChildren
      end
end

(***********************************)
(******** Context Criterion ********)
(***********************************)

(** Count the number of LVals in an expression *)
class countLvalExp = object(_)
  inherit Visitor.frama_c_inplace

  method get_LVals () = !visited

  method! vlval lval =
    match lval with
    | Var v,_ ->
      (** If the Lval is previously defined, and not already in the list *)
      if should_instrument v v.vid then
        visited := v.vid :: !visited;
      Cil.SkipChildren
    | _ -> Cil.SkipChildren
end


(* This visitor visits each LVal to create sequences if the state t contains defs for this LVal *)
class visit_context (t:t) (current_stmt:stmt) = object(self)
  inherit Visitor.frama_c_inplace

  val sid = current_stmt.sid

  (* Create for a given def the sequence def-use for the current LVal *)
  method private mkComb eid defs =
    let ids = Annotators.next () in
    let max = DefSet.cardinal defs + 1 in
    add_done_set (sid, eid) defs;
    let f i def =
      let s = mkSeq_aux ids (string_of_int def.varId) (i+1) max in
      if def.funId = -1 then begin
        if not (Hashtbl.mem to_add_cond def.stmtDef) then
          Hashtbl.add to_add_cond def.stmtDef (mkCond def.varId);
        replace_or_add_list to_add_defs def.stmtDef (ids,s)
      end
      else
        replace_or_add_list to_add_fun def.funId (ids,s)
    in
    List.iteri f (DefSet.elements defs);
    let u = mkSeq_aux ids "N/A" max max in
    replace_or_add_list to_add_uses sid (ids,u);
    add_to_hyperlabel (eid, 0) ids;
    incr nb_seqs

  (* Get all the definition of a Lval  *)
  method! vexpr expr =
    match t with
    | Bottom -> Cil.SkipChildren
    | NonBottom t ->
      let vExp = new countLvalExp in
      ignore(Cil.visitCilExpr (vExp :> Cil.cilVisitor) expr);
      let lvalIds = vExp#get_LVals () in
      if List.length lvalIds > 1 then begin
        let filter_lval lvid =
          DefSet.elements (DefSet.filter (fun def -> def.varId = lvid) t)
        in
        let sorted_defs_per_vid = List.map filter_lval lvalIds in
        let all_combs = make_combs expr sid sorted_defs_per_vid in
        List.iter (self#mkComb expr.eid) all_combs;
        Cil.SkipChildren
      end
      else
        Cil.SkipChildren
end


(******************************)
(***** Dataflow analysis ******)
(******************************)

module type V_type = sig
  val make : t -> stmt -> Cil.cilVisitor
end

let get_v b =
  if b then
    (module struct
      let make state stmt = (new visit_defuse state stmt :> Cil.cilVisitor)
    end: V_type)
  else
    (module struct
      let make state stmt = (new visit_context state stmt :> Cil.cilVisitor)
    end: V_type)

module P(V : V_type) = struct

  let print_elt elt =
    Printf.printf "funId: %d / varId: %d / defId: %d / stmtDef: %d\n%!"
      elt.funId elt.varId elt.defId elt.stmtDef

  let print_defs set =
    DefSet.iter (fun elt -> Printf.printf "    %!"; print_elt elt) set

  let pretty _ = function
    | Bottom -> Printf.printf "Bottom\n%!"
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

  (* For each definition statement, change the current state by
     adding or not new definition, removing older ones etc... *)
  let do_def vid offset sid = function
    | Bottom -> Bottom
    | NonBottom t ->
      let vid = get_vid_with_field vid offset in
      let t_clean =
        if Options.CleanDataflow.get () then
          remove_def vid t
        else
          t
      in
      let new_t =
        if Hashtbl.mem seen_def sid then
          DefSet.add (Hashtbl.find seen_def sid) t_clean
        else begin
          let defId = get_next_def_id vid in
          let new_def = make_def vid defId sid in
          Hashtbl.add seen_def sid new_def;
          DefSet.add new_def t_clean
        end
      in
      NonBottom new_t

  (* Function called for each stmt and propagating new states to each succs of stmt *)
  let transfer_stmt stmt state =
    match stmt.skind with
    | Instr i when not (Utils.is_label i) ->
      visited := [];
      ignore(Cil.visitCilInstr (V.make state stmt) i);
      begin match i with
        | Set ((Var v,offset),_,_)
        | Call (Some (Var v,offset),_,_,_) ->
          if not (v.vname = "__retres") && not v.vtemp then begin
            let res = do_def v.vid offset stmt.sid state in
            List.map (fun x -> (x,res)) stmt.succs
          end
          else List.map (fun x -> (x,state)) stmt.succs
        | Local_init (v,_,_) ->
          if not (v.vname = "__retres") && not v.vtemp then begin
            let res = do_def v.vid NoOffset stmt.sid state in
            List.map (fun x -> (x,res)) stmt.succs
          end
          else List.map (fun x -> (x,state)) stmt.succs
        | _ -> List.map (fun x -> (x,state)) stmt.succs
      end
    | Return (Some e,_) ->
      visited := [];
      ignore(Cil.visitCilExpr (V.make state stmt) e);
      List.map (fun x -> (x,state)) stmt.succs
    | If (e,_,_,_)
    | Switch (e,_,_,_) ->
      visited := [];
      ignore(Cil.visitCilExpr (V.make state stmt) e);
      let all_b = Kernel_function.find_all_enclosing_blocks stmt in
      clean_equivalent_tbl all_b;
      List.map (fun x -> (x,state)) stmt.succs
    | _ -> List.map (fun x -> (x,state)) stmt.succs

end

(* For each function, do a dataflow analysis and create sequences
   Initial state contains def of each formal
*)
let do_function kf defuse =
  let module Fenv = (val Dataflows.function_env kf) in
  let module V = (val (get_v defuse)) in
  let module Inst = P(V) in
  let args = Kernel_function.get_formals kf in
  let first_stmt = Kernel_function.find_first_stmt kf in
  let funId = Kernel_function.get_id kf in
  let f acc arg =
    let defId = get_next_def_id arg.vid in
    DefSet.add (make_def ~funId arg.vid defId first_stmt.sid) acc
  in
  let init = List.fold_left f DefSet.empty args in
  let module Arg = struct
    include Inst
    let init =
      [(first_stmt, NonBottom init)]

  end in
  let module _ = Dataflows.Simple_forward(Fenv)(Arg) in
  ()

(* This visitor will, for each function :
   - Do the dataflows analysis and fill hashtbls
   - Then visit the body of the function, add add all sequences to where
     they belong.
   - Clean all hashtbls and do the next function (if any)
*)
class addSequences defuse= object(self)
  inherit Visitor.frama_c_inplace

  (* get all sequences, sort defs and uses by sequence ID, and returns
     a pair of list (before,after) with sequences to add before & after the current statement *)
  method private get_seqs_sorted sid =
    let defs =
      if Hashtbl.mem to_add_defs sid then
        Hashtbl.find to_add_defs sid
      else []
    in
    let uses =
      if Hashtbl.mem to_add_uses sid then
        Hashtbl.find to_add_uses sid
      else []
    in
    let cond =
      if Hashtbl.mem to_add_cond sid then
        [Hashtbl.find to_add_cond sid]
      else []
    in
    let compare (id1,_) (id2,_) = compare id1 id2 in
    let defs,uses = List.sort compare defs, List.sort compare uses in
    List.map (fun (_,s) -> s) defs, (List.map (fun (_,s) -> s) uses) @ cond

  method! vfunc (dec : fundec) : fundec Cil.visitAction =
    let kf = Extlib.the self#current_kf in
    if Kernel_function.is_definition kf && Annotators.shouldInstrument dec.svar then begin
      Cfg.clearCFGinfo ~clear_id:false dec;
      Cfg.cfgFun dec;
      do_function kf defuse;
      Cil.DoChildrenPost (fun f ->
          let id = dec.svar.vid in
          let defs = List.sort compare (
              if Hashtbl.mem to_add_fun id then
                Hashtbl.find to_add_fun id
              else [])
          in
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
let symb : string ref = ref "."

(** Create all hyperlabels *)
let compute_hl () : string =
  if "-" = !symb then
    Hashtbl.fold (fun _ seqs str ->
        let seqs = List.sort compare seqs in
        List.fold_left (fun acc s -> acc ^ Annotators.next_hl() ^ ") <s" ^ string_of_int s ^"|; ;>,\n") str seqs
      ) hyperlabels ""
  else
    Hashtbl.fold (fun _ seqs str ->
        let seqs = List.sort compare seqs in
        str ^ Annotators.next_hl() ^ ") <" ^ (String.concat !symb (List.map (fun s -> "s" ^ string_of_int s) seqs)) ^ "|; ;>,\n"
      ) hyperlabels ""

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl () in
  let out = open_out_gen [Open_creat; Open_append] 0o644 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Number of ignored labels %d" !ignoredLabels;
  Options.feedback "Total number of sequences = %d" !nb_seqs;
  Options.feedback "Total number of hyperlabels = %d" (Annotators.getCurrentHLId())

(** Successively pass the 2 visitors *)
let visite (file : file) visit : unit =
  Visitor.visitFramacFileSameGlobals (new addSequences visit :> Visitor.frama_c_visitor) file;
  Cfg.clearFileCFG ~clear_id:false file;
  Cfg.computeFileCFG file;
  Ast.mark_as_changed ();

  (** All-defs annotator *)
module ADC = Annotators.Register (struct
    let name = "ADC"
    let help = "All-Definitions Coverage"
    let apply _ file =
      visite file true;
      symb := "+";
      gen_hyperlabels ()
  end)

(** All-uses annotator *)
module AUC = Annotators.Register (struct
    let name = "AUC"
    let help = "All-Uses Coverage"
    let apply _ file =
      visite file true;
      symb := ".";
      gen_hyperlabels ()
  end)

(** Def-Use annotator *)
module DUC = Annotators.Register (struct
    let name = "DUC"
    let help = "Definition-Use Coverage"
    let apply _ file =
      visite file true;
      symb := "-";
      gen_hyperlabels ()
  end)

(**
   Context criteria annotator
*)
module Context = Annotators.Register (struct
    let name = "CTXC"
    let help = "Context Coverage"
    let apply _ file =
      Options.result "[WIP] Context is currently in Alpha";
      visite file false;
      gen_hyperlabels ()
  end)
