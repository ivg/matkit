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


let index_of_char char =
  match Char.get_digit char with
  | Some n -> INum (Nat1.of_int_exn n)
  | None   -> IVar (Char.to_string char)


let type_of_string str = match String.to_list str with
  | [n] -> index_of_char n, INum One
  | [n; ','; m] -> index_of_char n, index_of_char m
  | _  -> invalid_arg "type := n,m | n"

let string_of_index = function
  | IVar v -> v
  | INum n -> Nat1.to_string n

let string_of_type = function
  | n, INum One -> string_of_index n
  | n, m -> string_of_index n ^ "," ^ string_of_index m


let assert_type  exp subexp texp =
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
  let x,y,z = Exp.(var 'x', var 'y', var 'z') in
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
