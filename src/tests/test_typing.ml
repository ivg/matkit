

open Core.Std
open Ast
open Type

let () =
  let exp = Exp.(tran (var 'x') * var 'A' * var 'x') in
  let ty = Typing.infer [] exp in
  printf "Done. Infered type is:\n %s"
    (Sexp.to_string_hum (Exp.Map.sexp_of_t sexp_of_ty ty))

