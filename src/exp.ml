open Core.Std
open Ast

let var s = Var s
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

module ExpPrinter = struct
 open Printer
 (** Helper functions for pretty printing **)

 (** this overrides binop for truly assosiative operators, such as
     multiplication, addition, i.e, such operations that:
     a @ b @ c = (a @ b) @ c = a @ (b @ c).
     The reason to introduce this override was juxtaposition. Since we
     introduced it in our grammar as a right recursive rule, it parses
     [ABCD] as [A(B(CD)], although it is mathematically correct, it
     leads to an ugly printing.

     Note: With this printer we cannot assume that div and mul have
     the same precedence, and rely on assosiativity to rule out the
     proper order of application (since division is not truly
     assosiative). Thats why I reduced div precedence below
     multiplication.

     Note: this all should be highly tested!
  *)
 let assoc_binop : level -> op:ppr -> ppr -> ppr -> ppr =
   fun lev ~op:sep l r ->
     parenbox Left lev (level lev (left l ++ sep ++ left r))

 (** BINARY OPERATORS **)


 let eql_lev = 0.8
 let add_lev = 1.0
 let div_lev = 1.5
 let mul_lev = 2.0
 let pow_lev = 3.0

 let mbinop lev sep = binop Left lev ~op:(string sep)
 (** All matlan binops are left associative except equals **)


 let ( =.  )  = binop Noassoc eql_lev ~op:(space ++ string "=" ++ space)
 let ( +   )  = mbinop add_lev "+"
 let ( -   )  = mbinop add_lev "-"
 let ( *   )  = assoc_binop mul_lev ~op:nop
 let ( *.  ) = mbinop mul_lev ".*"
 let ( /   )  = mbinop div_lev "/"
 let ( /.  ) = mbinop div_lev "./"
 let ( **  )  = mbinop pow_lev "^"
 let ( **. ) = mbinop pow_lev ".^"

 (** Unary Operators **)
 let mprefix lev op = prefix lev ~op:(string op)
 let mpostfix lev op = postfix lev ~op:(string op)

 let neg_lev = 5.0
 let inv_lev = 4.0

 let neg  = mprefix neg_lev "~"
 let inv  = mpostfix inv_lev "`"
 let tran = mpostfix inv_lev "'"
 let conj = mpostfix inv_lev "\""
end

let is_inv expr =
  match expr with
  | Bop(Pow, expr', Num (-1.0)) -> true
  | _ -> false

let rec ppr expr : ppr  =
  let open Printer in
  let open ExpPrinter in
  match expr with
  | Num n -> string (sprintf "%g" n)
  | Var v -> string v
  | Uop (op, expr') ->
    (match op with
    | Tran -> tran (ppr expr')
    | Conj -> conj (ppr expr')
    | UNeg -> neg (ppr expr'))
  | Bop (op, expr1, expr2) ->
    (if (is_inv expr) then (inv (ppr expr1)) else
    let expr1_ppr = ppr expr1 in
    let expr2_ppr = ppr expr2 in
    match op with
    | Eql  -> expr1_ppr =.  expr2_ppr
    | Add  -> expr1_ppr +   expr2_ppr
    | Sub  -> expr1_ppr -   expr2_ppr
    | Mul  -> expr1_ppr *   expr2_ppr
    | Had  -> expr1_ppr *.  expr2_ppr
    | Div  -> expr1_ppr /   expr2_ppr
    | HDiv -> expr1_ppr /.  expr2_ppr
    | Pow  -> expr1_ppr **  expr2_ppr
    | HPow -> expr1_ppr **. expr2_ppr)
  | Ind (expr', dim1, dim2) -> failwith "Todo"

let ppr_list exprs = failwith "Todo"

module T = struct
  type t = exp with sexp,compare
  let hash = Hashtbl.hash
end
include T
include Comparable.Make(T)
include Hashable.Make(T)
