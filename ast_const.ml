open Cil_types

let unk_loc = Cil_datatype.Location.unknown

module Exp = struct
  let mk ?(loc=unk_loc) = Cil.new_exp ~loc
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
  (* XXX let unop = *)
  let binop ?(loc=unk_loc) op left right =
    Cil.mkBinOp ~loc op left right

  (** Joins some expressions (at least one) with a binary operator. *)
  let join ?(loc=Cil_datatype.Location.unknown) op l =
    match l with
    | [] -> invalid_arg "join"
    | head :: tail ->
      List.fold_left (fun acc e -> Cil.mkBinOp loc op acc e) head tail

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
