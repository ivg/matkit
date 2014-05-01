open Core.Std
open Ast

type t = decl with sexp,compare

type ring_decl = d1:Dim.t -> d2:Dim.t -> t
let is_real s = s, Ring (R,None)
let is_integer s = s, Ring (Z,None)
let is_complex s = s, Ring (C,None)

let ring s ~r ~d1 ~d2 = s, Ring(r,Some (d1,d2))
let real = ring ~r:R
let integer = ring ~r:Z
let complex = ring ~r:C


let kind s ~is = s, Kind is

let matrix s = kind s ~is:"matrix"
let vector s = kind s ~is:"vector"
let scalar s = kind s ~is:"scalar"

let assoc s = List.map ~f:(fun prop -> prop s)
