(** Helpers to build expression.
    Example:
      Expression $Ax = b$, can be encoded with:
      [Exp.(Var 'A' * Var 'x' + Var 'b')]
  *)
open Core.Std
open Ast

type t = exp

val var: char -> t
val ( * ): t -> t -> t
val ( + ): t -> t -> t
val ( - ): t -> t -> t
val ( *. ): t -> t -> t
val ( ** ): t -> t -> t
val neg: t -> t
val tran: t -> t
val conj: t -> t
val to_string: t -> string

include Hashable.S with type t := t
include Comparable.S with type t := t
