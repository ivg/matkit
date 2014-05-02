open Core.Std
open Ast
open Type

(** Environment is a symbol table *)
module Table = Exp.Table
type t = ty Table.t with sexp

let empty () : t = Table.create ()

(** [next_char c] returns next a lowercase character following the
    [c], if it exists   *)
let next_char char : char option =
  match (Char.to_int char + 1) |> Char.of_int with
  | Some c when Char.(is_lowercase c && is_alpha c) -> Some c
  | _ -> None

(** [next_sym s] generates new fresh symbol, assuming that [s] is the
    highest generated symbol.  The function will at first use all
    lowercase symbols from [[a-z]], and then fall back to a simple
    scheme [t_n], where n goes from zero to infinity.
*)
let next_sym sym : sym =
  match Sym.split sym ~on:'_' with
  | [v] when Sym.length v = 1 ->
    (match next_char v.[0] with
     | Some c -> Sym.of_char c
     | None -> "t_1")
  | [v;n] -> let m = Int.of_string n + 1 in
    sprintf "%s_%d" v m
  | _ -> assert false

let min_sym = 'a'

let add env sym ty = Table.add_exn env ~key:sym ~data:ty

let add_fresh (env : t) sym : ty =
  let fresh_var =
    let max_var =
      Table.fold env ~init:(Sym.of_char min_sym)
        ~f:(fun ~key:_ ~data:(d1,d2) s0 ->
            match List.filter [d1;d2] ~f:Dim.is_var with
            | [d1;d2] -> Sym.max s0 (Dim.to_sym (Dim.max d1 d2))
            | [d1] -> Sym.max (Dim.to_sym d1) s0
            | _  -> s0) in
    let i1 = next_sym max_var in
    IVar i1, IVar (next_sym i1) in
  add env sym fresh_var;
  fresh_var

let get_or_add_fresh (env:t) sym : ty =
  match Table.find env sym with
  | Some ty -> ty
  | None -> add_fresh env sym


let find env = Table.find env
let is_bound env = Table.mem env

let is_num = function
  | INum _ -> true
  | _ -> false

let has_sym d = not (is_num d)

let bound_dims env =
  List.fold (Table.data env) ~init:Dim.Set.empty ~f:(fun s (d1,d2) ->
      match List.filter [d1;d2] ~f:has_sym with
      | [s1;s2] -> Set.add (Set.add s s1) s2
      | [s1] -> Set.add s s1
      | _ -> s)

let create_substitution env ds  =
  let module Map = Exp.Map in
  let (init : ty Map.t) = Map.empty in
  Table.fold env ~init ~f:(fun ~key:expr ~data:(t1,t2) map ->
      let ty = match UnionFind.(find ds t1, find ds t2) with
        | Some t1, Some t2 -> (t1,t2)
        | Some t1, None    -> (t1,t2)
        | None,    Some t2 -> (t1,t2)
        | None,    None    -> (t1,t2) in
      Map.add map ~key:expr ~data:ty)
