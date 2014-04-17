(** Helpers to build expression.
    Example:
      Expression $Ax = b$, can be encoded with:
      [Exp.(Var 'A' * Var 'x' + Var 'b')]
  *)
open Ast

type t = Ast.t

val var: char -> t
val ( * ): t -> t -> t
val ( + ): t -> t -> t
val ( - ): t -> t -> t
val ( *. ): t -> t -> t
val ( ** ): t -> t -> t
val tran: t -> t
val conj: t -> t
val to_string: t -> string
