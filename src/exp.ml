open Core.Std
open Ast

let var s = Var (Sym.of_char s)
let is_var = function
  | Var _ -> true
  | _ -> false

let num n = Num n
let int n = Num (Float.of_int n)
let ( * ) t1 t2  = Bop (Mul,t1,t2)
let ( *. ) t1 t2  = Bop (Had,t1,t2)
let ( - ) t1 t2  = Bop (Sub,t1,t2)
let ( + ) t1 t2  = Bop (Add,t1,t2)
let ( ** ) t1 t2 = Bop (Pow,t1,t2)
let ( **. ) t1 t2 = Bop (HPow,t1,t2)
let ( /. ) t1 t2 = Bop (HDiv, t1, t2)
let neg t = Uop (UNeg, t)
let tran t = Uop (Tran, t)
let conj t = Uop (Conj, t)
let to_string t = Sexp.to_string_hum (sexp_of_exp t)

let fold t ~init ~f =
  let rec loop a expr =
    let a = f expr a in
    match expr with
    | Num _ | Var _ -> a
    | Uop (_,expr) | Ind (expr,_,_) -> loop a expr
    | Bop (_,e1,e2) ->
      let a = loop a e1 in
      loop a e2 in
  loop init t


module T = struct
  type t = exp with sexp,compare
  let hash = Hashtbl.hash
end
include T
include Comparable.Make(T)
include Hashable.Make(T)
