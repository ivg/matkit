open Core.Std
open Ast
open Debug


module Set = Dim.Set
type t = Set.t list with sexp



let create () = []

let get t s = List.find t ~f:(fun ss -> Set.mem ss s)

let remove ss t =
  List.filter t ~f:(fun ss' -> not (Set.equal ss ss'))

let union t s1 s2 : t =
  match get t s1, get t s2 with
  | Some ss1, Some ss2 ->
    Set.union ss1 ss2 :: (t |> remove ss1 |> remove ss2)
  | Some ss, None -> Set.add ss s2 :: remove ss t
  | None, Some ss -> Set.add ss s1 :: remove ss t
  | None, None -> Set.add (Set.singleton s1) s2 :: t

let is_var = function
  | IVar _ -> true
  | IConst _ | INum _ -> false

let is_const v = not (is_var v)
let has_consts ss = Set.exists ss ~f:is_const

(** [nth_dims used n] finds an appropriate constant name for an nth
    group of variables. At first it tries to use nice names. If all
    names are already used then it creates a fresh name from an
    indexed T constant, checking that a new name is free. *)
let nth_dim used : Dim.mapping =
  let used = List.fold used ~init:Set.empty ~f:Set.union in
  Dim.create_mapping ~const:true ~used

let find t x : dim =
  let nums,vars = List.partition_tf t ~f:has_consts in
  match List.findi vars ~f:(fun _ ss -> Set.mem ss x) with
  | Some (n,_) -> nth_dim nums n
  | None -> match List.find nums ~f:(fun ss -> Set.mem ss x) with
    | None ->
      eprintf "Failed to find %s in\n%s\n"
        (Dim.to_string x)
        (Sexp.to_string_hum (sexp_of_t t));
      raise Not_found
    | Some ss -> match Set.to_list (Set.filter ss ~f:is_const) with
      | [] -> assert false
      | [r] -> r
      | r1::r2::_ -> assert false
