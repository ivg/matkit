open Core.Std
open Ast

let () =
  let exp = Exp.(Var "A" * Var "x" + Var "b") in
  let exp = Exp.(exp * exp) in
  printf "exp = %s\n" (Exp.to_string exp)

let () =
  let exp_ppr = Exp.ppr (Exp.((Var "A" + Var "B") * Var "C")) in
  let res = Printer.Token.show (exp_ppr Noassoc 0.0) in
  printf "result = %s\n" res
