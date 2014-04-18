

open Core.Std
open Ast
open Type

let () =
  let exp = Exp.(Var "x" + Var "A" * Var "x" + Var "y") in
  let ty = Typing.infer [] exp in
  printf "Done. Infered type is %s"
    (Sexp.to_string_hum (Sym.Map.sexp_of_t sexp_of_ty ty))

