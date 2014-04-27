open Core.Std
open Ast
open Type

let verbose = true

let typeof exp subexp =
  let subst = Typing.infer [] exp in
  match Exp.Map.find subst subexp with
  | Some ty -> ty
  | None ->
    let msg = sprintf "%s is not a subexpression of %s"
        (Exp.to_string subexp) (Exp.to_string exp) in
    invalid_arg msg

let one = INum Nat1.one

let type_of_string str =
  match String.split ~on:',' str with
  | [n] -> Dim.of_string n, one
  | [n; m] -> Dim.of_string n, Dim.of_string m
  | _  -> invalid_arg "type := n,m | n"

let string_of_type = function
  | n, INum m when Nat1.(m = one) -> Dim.to_string n
  | n, m -> Dim.to_string n ^ "," ^ Dim.to_string m


let assert_type exp subexp texp =
  let ty1 = typeof exp subexp in
  let ty2 = type_of_string texp in
  let eq = Type.(ty1 = ty2) in
  let msg r = sprintf "Assertion: %s = %s %s in,\n%s of %s"
      (string_of_type ty1)
      (string_of_type ty2) r
      (Exp.to_string exp)
      (Exp.to_string subexp) in
  if not eq then failwith (msg "failed");
  if verbose then printf "%s\n" (msg "passed")


let () =
  let x,_,_ = Exp.(var 'x', var 'y', var 'z') in
  let a,b,c = Exp.(var 'A', var 'B', var 'C') in
  let _2,_3 = Exp.(num 2, num 3) in
  let exp1 = Exp.(tran x * a * x) in
  assert_type exp1 exp1 "1";
  assert_type exp1 a "N,N";
  assert_type exp1 x "N";
  assert_type exp1 Exp.(tran x * a) "1,N";

  let exp2 = Exp.(a * a - a * a) in

  assert_type exp2 exp2 "N,N";

  let exp3 = Exp.(a*b*c) in
  assert_type exp3 exp3 "P,Q";
  assert_type exp3 a "P,M";
  assert_type exp3 b "M,N";
  assert_type exp3 c "N,Q"
