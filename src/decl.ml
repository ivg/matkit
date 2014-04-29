open Core.Std
open Ast
open Nat1

let kind_list_of_props (s: sym) (lst: property list) =
  List.map ~f:(fun prop -> (s,prop)) lst

let ring r ?(dimensions = (Empty, Empty)) =
  let ring_t = Ring.ring_of_str r in
  let conv_dim d = 
    match d with
    | Const_dim d' -> INum (Nat1.of_int_exn d')
    | Var_dim d' -> IVar d'
    | Rigid_dim d' -> IConst d'
    | Empty -> INum one
  in
  let get_dims =
    match dimensions with
    | (Empty, Empty) -> None
    | (d1, d2) -> Some(conv_dim d1, conv_dim d2)
  in
  Ring(ring_t, get_dims dimensions)

let kind k = Kind(k)
let decl s lst = kind_list_of_props s lst
  
