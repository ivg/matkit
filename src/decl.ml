open Core.Std
open Ast

type t = Ast.decls with sexp,compare

let create s = List.map ~f:(fun prop -> (s,prop))
let ring r d = Ring (Ring.ring_of_str r, d)
let kind k = Kind k
