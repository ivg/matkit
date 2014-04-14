open Core.Std
open Ast

type t = Ast.t
let var s = Var s
let ( * ) t1 t2  = Bop (Mul,t1,t2)
let ( *. ) t1 t2  = Bop (Had,t1,t2)
let ( - ) t1 t2  = Bop (Sub,t1,t2)
let ( + ) t1 t2  = Bop (Add,t1,t2)
let ( ** ) t1 t2 = Bop (Pow,t1,t2)
let tran t = Uop (Tran, t)
let conj t = Uop (Conj, t)
let to_string t = Sexp.to_string_hum (sexp_of_t t)
