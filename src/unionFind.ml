open Core.Std
open Ast


let nth_dim n =
  let dims = [| "N"; "M"; "P"; "Q"; "K"; "L" |] in
  let sym =
    if n < Array.length dims
    then dims.(n)
    else sprintf "T_%d" (n-Array.length dims+1) in
  IVar sym

module Set = Index.Set
type t = Set.t list


let create () = []


let get t s = List.find t ~f:(fun ss -> Set.mem ss s)

let remove ss t =
  List.filter t ~f:(fun ss' -> not (Set.equal ss ss'))

let union t s1 s2 : t =
  match get t s1, get t s2 with
  | Some ss1, Some ss2 ->
    (Set.union ss1 ss2) ::  t |> remove ss1 |> remove ss2
  | Some ss, None -> Set.add ss s2 :: remove ss t
  | None, Some ss -> Set.add ss s1 :: remove ss t
  | None, None -> Set.add (Set.singleton s1) s2 :: t

let is_number = function
  | IVar _ -> false
  | INum _ -> true

let has_numbers ss = Set.exists ss ~f:is_number

let find t x =
  let nums,vars = List.partition_tf t ~f:has_numbers in
  match List.findi vars ~f:(fun _ ss -> Set.mem ss x) with
  | Some (n,_) -> nth_dim n
  | None -> match List.find nums ~f:(fun ss -> Set.mem ss x) with
    | None -> raise Not_found
    | Some ss -> match Set.to_list (Set.filter ss ~f:is_number) with
      | [idx] -> idx
      | _ -> assert false
