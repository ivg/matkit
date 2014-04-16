open Core.Std
open Ast
open Type

(** Environment is a symbol table *)
module Table = Sym.Table
type t = ty Table.t with sexp


let empty () : t = Table.create ()

let min_sym = 'a'

(** a stub to generate fresh type variables  *)
let next_char char : char option =
  match (Char.to_int char + 1) |> Char.of_int with
  | Some c when Char.(is_lowercase c && is_alpha c) -> Some char
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
        ~f:(fun ~key:_ ~data max_sym -> match data with
            | TVar id -> if id < max_sym then max_sym else id
            | _ -> (max_sym : sym)) in
    next_sym max_var in
  let ty = TVar fresh_var in
  Table.add_exn env ~key:sym ~data:ty;
  ty

let get_or_add_fresh (env:t) sym : ty =
  match Table.find env sym with
  | Some ty -> ty
  | None -> add_fresh env sym
