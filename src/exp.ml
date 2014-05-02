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

open Printer
(** Helper functions for pretty printing **)
(** Nums and Vars **)
let var v = string v
let num n = string (Float.to_string n)

(** BINARY OPERATORS **)

let mbinop lev sep = binop Left lev ~op:(string sep)
(** All matlan binops are left associative except equals **)

let eql_lev = 0.8
let add_lev = 1.0
let mul_lev = 2.0
let pow_lev = 3.0

let eql  = binop Noassoc eql_lev ~op:(space ++ string "=" ++ space)
let add  = mbinop add_lev "+" 
let sub  = mbinop add_lev "-"
let mul  = mbinop mul_lev ""
let hmul = mbinop mul_lev "*."
let div  = mbinop mul_lev "/"
let hdiv = mbinop mul_lev "/."
let pow  = mbinop pow_lev "^"
let hpow = mbinop pow_lev "^." 

(** Unary Operators **)
let mprefix lev op = prefix lev ~op:(string op)
let mpostfix lev op = postfix lev ~op:(string op)

let neg_lev = 5.0
let inv_lev = 4.0

let neg  = mprefix neg_lev "~"
let inv  = mpostfix inv_lev "`"
let tran = mpostfix inv_lev "/'"
let conj = mpostfix inv_lev "^C" (** placeholder until representation is decided **)
s

let is_inv expr =
  match expr with
  | Bop(Pow, expr', Num (-1.0)) -> true
  | _ -> false

let ppr_unop op = function
  | Tran -> tran
  | Conj -> conj
  | Uneg -> neg
and
let ppr_binop op = function
    | Eql  -> eql
    | Add  -> add
    | Sub  -> sub
    | Mul  -> mul
    | Had  -> had
    | Div  -> div
    | HDiv -> hdiv
    | Pow  -> pow
    | HPow -> hpow
and
let rec ppr expr = function
  | Num n -> num n
  | Var v -> var v
  | Uop (op, expr') ->
    (ppr_unop op) ++ (ppr expr')
  | Bop (op, expr1, expr2) -> 
    if (is_inv expr) then (inv ++ ppr expr1) else
    (ppr expr1) ++ (ppr_binop op) ++ (ppr expr2)
  | Ind (expr', dim1, dim2) -> failwith "Todo"

module T = struct
  type t = exp with sexp,compare
  let hash = Hashtbl.hash
end
include T
include Comparable.Make(T)
include Hashable.Make(T)
