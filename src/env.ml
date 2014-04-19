open Core.Std
open Ast
open Type

(** Environment is a symbol table *)
module Table = Exp.Table
type t = ty Table.t with sexp

let empty () : t = Table.create ()

let min_sym = 'a'

(** a stub to generate fresh type variables  *)
let next_char char : char option =
  match (Char.to_int char + 1) |> Char.of_int with
  | Some c when Char.(is_lowercase c && is_alpha c) -> Some c
  | _ -> None

let next_sym sym : sym =
  match Sym.to_list sym |> List.rev with
  | [] -> assert false
  | t::hs -> match next_char t with
    | Some t -> List.rev (t::hs) |> Sym.of_char_list
    | None   -> min_sym :: t :: hs |> List.rev |> Sym.of_char_list

let add_fresh (env : t) sym : ty =
  let fresh_var =
    let max_var =
      Table.fold env ~init:(Sym.of_char min_sym)
        ~f:(fun ~key:_ ~data s0 -> match data with
            | IVar s1, IVar s2 -> Sym.max s0 (Sym.max s1 s2)
            | IVar s1,_|_,IVar s1 -> Sym.max s1 s0
            | INum _, INum _ -> s0) in
    let i1 = next_sym max_var in
    IVar i1, IVar (next_sym i1) in
  let ty = fresh_var in
  Table.add_exn env ~key:sym ~data:ty;
  ty

let get_or_add_fresh (env:t) sym : ty =
  match Table.find env sym with
  | Some ty -> ty
  | None -> add_fresh env sym


let find env = Table.find env
let is_bound env = Table.mem env

let create_substitution env ds  =
  let module Map = Exp.Map in
  let (init : ty Map.t) = Map.empty in
  Table.fold env ~init ~f:(fun ~key:expr ~data:(t1,t2) map ->
      let t1 = UnionFind.find ds t1 in
      let t2 = UnionFind.find ds t2 in
      Map.add map ~key:expr ~data:(t1,t2))
