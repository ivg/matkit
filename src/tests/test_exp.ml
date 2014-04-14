open Core.Std
open Ast

let () =
  let exp = Exp.(Var 'A' * Var 'x' + Var 'b') in
  let exp = Exp.(exp * exp) in
  printf "exp = %s\n" (Exp.to_string exp)
