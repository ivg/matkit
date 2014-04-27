open Core.Std
open Ast
open Debug


module Set = Dim.Set
type t = Set.t list



let create () = []

let get t s = List.find t ~f:(fun ss -> Set.mem ss s)

let remove ss t =
  List.filter t ~f:(fun ss' -> not (Set.equal ss ss'))

let union t s1 s2 : t =
  match get t s1, get t s2 with
  | Some ss1, Some ss2 ->
    Set.union ss1 ss2 :: t |> remove ss1 |> remove ss2
  | Some ss, None -> Set.add ss s2 :: remove ss t
  | None, Some ss -> Set.add ss s1 :: remove ss t
  | None, None -> Set.add (Set.singleton s1) s2 :: t

let is_var = function
  | IVar _ -> true
  | IConst _ | INum _ -> false

let is_const v = not (is_var v)
let has_consts ss = Set.exists ss ~f:is_const

exception Type_error of dim * dim with sexp

let type_error d1 d2 = raise (Type_error (d1,d2))

(** [nth_dims used n] finds an appropriate constant name for an nth
    group of variables. At first it tries to use nice names. If all
    names are already used then it creates a fresh name from an
    indexed T constant, checking that a new name is free. *)
let nth_dim used n : dim =
  let is_used d = List.exists used ~f:(fun s -> Set.mem s (IConst d)) in
  let is_free d = not (is_used d) in
  let dims =
    List.filter [ "N"; "M"; "P"; "Q"; "K"; "L" ] ~f:is_free |>
    Array.of_list in
  let sym =
    if n < Array.length dims
    then dims.(n)
    else
      let nth n = sprintf "T_%d" (n-Array.length dims+1) in
      let rec search_free n =
        let var = nth n in
        if is_free var then var else search_free (n+1) in
      search_free n in
  IConst sym

let find t x : dim =
  let nums,vars = List.partition_tf t ~f:has_consts in
  match List.findi vars ~f:(fun _ ss -> Set.mem ss x) with
  | Some (n,_) -> nth_dim nums n
  | None -> match List.find nums ~f:(fun ss -> Set.mem ss x) with
    | None -> raise Not_found
    | Some ss -> match Set.to_list (Set.filter ss ~f:is_const) with
      | [] -> assert false
      | [r] -> r
      | r1::r2::_ -> type_error r1 r2
