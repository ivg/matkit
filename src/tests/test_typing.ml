

open Core.Std
open Ast
open Type

let () =
  let exp = Exp.(Var "A" * Var "x") in
  let env = Env.empty () in
  let ty,cs = Typing.recon env exp in
  printf "%s : %s\n s.t.\n%s\nenv = \n%s"
    (Exp.to_string exp)
    (Sexp.to_string_hum (sexp_of_ty ty))
    (Sexp.to_string_hum (sexp_of_constrs cs))
    (Sexp.to_string_hum (Env.sexp_of_t env));
