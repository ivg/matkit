open Core.Std
open Ast
open Nat1

type t = decls
type d = dims

let kind_list_of_props (s: sym) (lst: property list) =
  List.map ~f:(fun prop -> (s,prop)) lst

let empty = []
let to_dim n = INum(Nat1.of_int_exn n)
let ring r d = Ring(Ring.ring_of_str r, d)
let kind k = Kind(k)
let decl s lst = kind_list_of_props s lst
let to_string d = ""
