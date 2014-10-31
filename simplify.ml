open Cil_types



type formula =
  | TAnd of formula * formula
  | TOr of formula * formula
  | TNot of formula
  | TAtom of int
  | TTrue
  | TFalse

type temp2 = [
  | `TAnd of temp2 * temp2
  | `TOr of temp2 * temp2
  | `TAtomPos of int
  | `TAtomNeg of int
  | `TTrue
  | `TFalse
]

let pretty_minterm fmt minterm =
  let f e =
    Format.pp_print_char fmt begin
      match e with
      | `True -> '1'
      | `False -> '0'
      | `Dontcare -> 'x'
    end
  in
  List.iter f minterm

let pretty_dnf fmt dnf =
  let f minterm =
    pretty_minterm fmt minterm;
    Format.pp_print_char fmt ' '
  in
  Format.pp_print_string fmt "[ ";
  List.iter f dnf;
  Format.pp_print_char fmt ']'


let rec ntimes n e =
  if n > 0 then e :: (ntimes (n-1) e)
  else []

let dnf_true n =
  [ ntimes n `Dontcare ]

let dnf_false (_n : int) =
  []

let dnf_or (n : int) a b =
  assert (List.for_all (fun minterm -> n = List.length minterm) a);
  assert (List.for_all (fun minterm -> n = List.length minterm) b);
  List.append a b

let dnf_and_extbools a b =
  match a, b with
  | `False, `True | `True, `False -> None
  | `True, (`Dontcare | `True) | `Dontcare, `True -> Some `True
  | `False, (`Dontcare| `False) | `Dontcare, `False -> Some `False
  | `Dontcare, `Dontcare -> Some `Dontcare

let dnf_and_minterms n aminterm bminterm =
  assert (n = List.length aminterm);
  assert (n = List.length bminterm);
  let f acc a b =
    match acc with
    | None -> None
    | Some acc ->
      match dnf_and_extbools a b with
      | Some res -> Some (res :: acc)
      | _ -> None
  in
  match List.fold_left2 f (Some []) aminterm bminterm with
  | Some res -> Some (List.rev res)
  | None -> None;;

let dnf_and_dnf_minterm (n: int) a bminterm =
  let f acc aminterm =
    match dnf_and_minterms n aminterm bminterm with
    | Some res -> res :: acc
    | None -> acc
  in
  let res = List.rev (List.fold_left f [] a) in
  (* Format.printf " %a . %a = %a@." pretty_dnf a pretty_minterm bminterm pretty_dnf res; *)
  res

let dnf_and (n: int) a b =
  let f acc minterm =
    dnf_or n (dnf_and_dnf_minterm n a minterm) acc
  in
  List.fold_left f [] b;;

let dnf_var (n: int) v value : [> `True |`False |`Dontcare] list list =
  assert (v >= 0 && v < n);
  [ (ntimes v `Dontcare) @ [ value ] @ (ntimes (n-v-1) `Dontcare) ]

let rec sndpass ?(neg=false) (t : formula) : temp2 =
  match t, neg with
  | TAnd (a, b), false ->
    `TAnd (sndpass ~neg a, sndpass ~neg b)
  | TAnd (a, b), true ->
    `TOr (sndpass ~neg a, sndpass ~neg b)
  | TOr (a, b), false ->
    `TOr (sndpass ~neg a, sndpass ~neg b)
  | TOr (a, b), true ->
    `TAnd (sndpass ~neg a, sndpass ~neg b)
  | TNot a, neg ->
    sndpass ~neg:(not neg) a
  | TAtom a, false ->
    `TAtomPos a
  | TAtom a, true ->
    `TAtomNeg a
  | TTrue, false -> `TTrue
  | TTrue, true -> `TFalse
  | TFalse, false -> `TFalse
  | TFalse, true -> `TTrue

let rec pretty_t2 f (x : temp2) =
  match x with
  | `TAnd (a, b) ->
      Format.fprintf f "( %a /\\ %a )" pretty_t2 a pretty_t2 b
  | `TOr (a, b) ->
      Format.fprintf f "( %a \\/ %a )" pretty_t2 a pretty_t2 b
  | `TAtomPos i ->
      Format.fprintf f "#%d" i
  | `TAtomNeg i ->
      Format.fprintf f "~#%d" i
  | `TTrue ->
      Format.fprintf f "True"
  | `TFalse ->
      Format.fprintf f "False"

let rec trdpass n t =
  let res =
  match t with
  | `TAnd (a, b) ->
      let a = trdpass n a in
      let b = trdpass n b in
      let res = dnf_and n a b in
      (* Printf.printf "%s . %s =(\n%s\n)\n" (Bes.string_of_dnf_expression a) (Bes.string_of_dnf_expression b) (Bes.string_of_dnf_expression res); *)
      res
  | `TOr (a, b) ->
      let a = trdpass n a in
      let b = trdpass n b in
      let res = dnf_or n a b in
      (* Printf.printf "%s + %s = (%s)\n" (Bes.string_of_dnf_expression a) (Bes.string_of_dnf_expression b) (Bes.string_of_dnf_expression res); *)
      res
  | `TAtomPos id -> dnf_var n id `True
  | `TAtomNeg id -> dnf_var n id `False
  | `TTrue -> dnf_true n
  | `TFalse -> dnf_false n
  in
  (* Format.printf "3rd pass for %a ->@.%a@." pretty_t2 t pretty_dnf res; *)
  res

let convert_back_minterm minterm : formula =
  let f ((i,acc) : int * formula list) atom =
    let acc = match atom with
    | `Dontcare -> acc
    | `True -> (TAtom i) :: acc
    | `False -> (TNot (TAtom i)) :: acc
    in
    (i+1, acc)
  in
  let _, rev = List.fold_left f (0, []) minterm in
  match rev with
  | [] -> TTrue
  | last :: rest ->
      List.fold_left (fun acc e -> TAnd (e, acc)) last rest;;

let convert_back_dnf dnf : formula =
  match List.rev dnf with
  | [] -> TFalse
  | head :: tail ->
    let head' = convert_back_minterm head in
    let f (acc : formula) e : formula =
      TOr (convert_back_minterm e, acc)
    in
    List.fold_left f head' tail

let simplify n t1 =
  let t2 = sndpass t1 in
  let dnf = trdpass n t2 in
  let optdnf, _exact =  Bes.auto_optimize dnf in
  convert_back_dnf optdnf

module type BOOLEAN_CONVERTIBLE = sig
  type t
  type info
  val convert : ?info:info -> t -> int*info*formula
  val convert_back : info:info -> formula -> t
end


module Make (C : BOOLEAN_CONVERTIBLE) = struct

  let simplify e =
    let n, info, t1 = C.convert e in
    let optt1 = simplify n t1 in
    C.convert_back info optt1

end

module Exp = Make (struct

  type t = Cil_types.exp
  module ExpH = Cil_datatype.ExpStructEq.Hashtbl
  type info = int ExpH.t * t array
  open Ast_const

  type atom_map = int ExpH.t;;

  let rec fstpass (h : atom_map) n l e : int * exp list * formula =
    match e.enode with
    | BinOp (LAnd, a, b, _) ->
      let (na, la, a') = fstpass h n l a in
      let (nb, lb, b') = fstpass h na la b in
      (nb, lb, TAnd (a', b'))
    | BinOp (LOr, a, b, _) ->
      let (na, la, a') = fstpass h n l a in
      let (nb, lb, b') = fstpass h na la b in
      (nb, lb, TOr (a', b'))
    | UnOp (LNot, e, _) ->
      let (n', l', e') = fstpass h n l e in
      (n', l', TNot e')
    | _ ->
      if ExpH.mem h e then
        (n, l, TAtom (ExpH.find h e))
      else
        (ExpH.add h e n; (n+1, e :: l, TAtom n));;

  let convert ?info e =
    let h, n, l = match info with
      | Some (h, arr) -> (h, Array.length arr, List.rev (Array.to_list arr))
      | None -> ExpH.create 12, 0, []
    in
    let n, l, tmp = fstpass h n l e in
    (n, (h, Array.of_list (List.rev l)), tmp)

  let rec convert_back arr (phi:formula) : t =
    match phi with
    | TAnd (a, b) ->
        Exp.binop LAnd (convert_back arr a) (convert_back arr b)
    | TOr (a, b) ->
        Exp.binop LOr (convert_back arr a) (convert_back arr b)
    | TNot a ->
        Exp.lnot (convert_back arr a)
    | TAtom i ->
        arr.(i)
    | TTrue ->
        Exp.one ()
    | TFalse ->
        Exp.zero ()

  let convert_back ~info:(_, arr) phi =
    convert_back arr phi
end)

let simplify_exp = Exp.simplify
