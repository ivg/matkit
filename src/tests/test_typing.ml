

open Core.Std
open Ast
open Type

let () =
  let exp = Exp.(Var "x" + Var "A" * Var "x") in
  let ty = Typing.infer [] exp in
  printf "Done. Infered type is %s"
    (Sexp.to_string_hum (sexp_of_ty ty))

