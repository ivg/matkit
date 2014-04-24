(** Helpers to build expression.
    Example:
      Expression $Ax = b$, can be encoded with:
      [Exp.(Var 'A' * Var 'x' + Var 'b')]
  *)
open Core.Std
open Ast

type t = exp

val var: char -> t
val num: int  -> t
val ( * ): t -> t -> t
val ( + ): t -> t -> t
val ( - ): t -> t -> t
val ( *. ): t -> t -> t
val ( ** ): t -> t -> t
val ( /. ): t -> t -> t
val ( **. ) : t -> t -> t
val neg: t -> t
val tran: t -> t
val conj: t -> t
val to_string: t -> string

val fold: t -> init:'a -> f:(t -> 'a -> 'a) -> 'a

(* Helper functions for parser.mly *)
val string_of_char: char -> string
val concat_char_list: char list -> string
val kind_list_of_strings: char -> string list -> kind list

include Hashable.S with type t := t
include Comparable.S with type t := t
