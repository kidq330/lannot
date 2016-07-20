open Cil_types

let unk_loc = Cil_datatype.Location.unknown

module Exp = struct
  let mk ?(loc=unk_loc) =
    Cil.new_exp ~loc

  let zero ?(loc=unk_loc) () =
    Cil.zero ~loc

  let one ?(loc=unk_loc) () =
    Cil.one ~loc

  let integer ?(loc=unk_loc) =
    Cil.integer ~loc

  let var ?(loc=unk_loc) varinfo =
    Cil.evar ~loc varinfo

  let lval ?(loc=Cil_datatype.Location.unknown) lval =
    Cil.new_exp ~loc (Lval lval)

  let mem ?(loc=unk_loc) ~addr ~off =
    Cil.new_exp ~loc (Lval (Cil.mkMem ~addr ~off))

  let lnot ?(loc=unk_loc) e =
    Cil.new_exp ~loc (UnOp (LNot, e, Cil.intType))

  let neg ?(loc=unk_loc) e =
    let oldt = Cil.typeOf e in
    let newt = Cil.integralPromotion oldt in
    (* make integral promotion explicit *)
    let e' = Cil.mkCastT e oldt newt in
    Cil.new_exp ~loc (UnOp (Neg, e', newt))

  let binop ?(loc=unk_loc) op left right =
    let l = if Cil.isPointerType (Cil.typeOf left) then Cil.new_exp ~loc (BinOp (Ne,left,(Cil.mkCast (Cil.zero ~loc) Cil.voidPtrType),Cil.intType)) else left in
    let r = if Cil.isPointerType (Cil.typeOf right) then Cil.new_exp ~loc (BinOp (Ne,right,(Cil.mkCast (Cil.zero ~loc) Cil.voidPtrType),Cil.intType)) else right in
    Cil.mkBinOp ~loc op l r

  let implies ?(loc=unk_loc) l r =
    let n_l = lnot ~loc l in
    Cil.mkBinOp ~loc LOr n_l r

  let iff ?(loc=unk_loc) l r =
    let l_imp_r = implies ~loc l r in
    let r_imp_l = implies ~loc r l in
    Cil.mkBinOp ~loc LAnd l_imp_r r_imp_l

  let niff ?(loc=unk_loc) l r =
    let nl_and_r = Cil.mkBinOp ~loc LAnd (lnot ~loc l) r in
    let l_and_nr = Cil.mkBinOp ~loc LAnd l (lnot ~loc r) in
    Cil.mkBinOp ~loc LOr nl_and_r l_and_nr

  let rec replace ~whole ~part ~(repl: exp) : exp=
    if part == whole then repl
    else
      match whole.enode with
      | UnOp (op, e, typ) ->
        let e' = replace e part repl in
        if e == e' then whole else mk ~loc:whole.eloc (UnOp (op, e', typ))
      | BinOp (op, e1, e2, typ) ->
        let e1' = replace e1 part repl in
        let e2' = replace e2 part repl in
        if e1 == e1' && e2 == e2' then whole else mk ~loc:whole.eloc (BinOp (op, e1', e2', typ))
      | _ -> whole


  (** Joins some expressions (at least one) with a binary operator. *)
  let rev_join ?(loc=Cil_datatype.Location.unknown) op l =
    match l with
    | [] -> invalid_arg "join"
    | head :: tail ->
      List.fold_left (fun acc e -> Cil.mkBinOp loc op e acc) head tail

  let join ?(loc=Cil_datatype.Location.unknown) op l =
    rev_join ~loc op (List.rev l)

  let copy = Cil.copy_exp
end

module Lval = struct
  let var = Cil.var
  let mem = Cil.mkMem
  let addOffset ~off ~base = Cil.addOffsetLval off base
end

module Stmt = struct
  let mk = Cil.mkStmt
  let block stmts = Cil.mkStmt (Block (Cil.mkBlock stmts))
end

module Block = struct
  let mk = Cil.mkBlock
end
