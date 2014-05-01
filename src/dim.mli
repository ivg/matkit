open Core.Std
open Ast

type t = dim with sexp,compare
include Comparable  with type t := t
include Hashable    with type t := t
include Stringable  with type t := t
val one: t
val of_int: int  -> t
val of_nat: nat1 -> t
val to_sym: t -> sym
val of_sym: sym -> t
val is_var: t -> bool
val is_num: t -> bool

type mapping = int -> t
(** [mapping] is a correspondance between natural numbers and
    instances of type t  *)

val create_mapping: const:bool -> used:Set.t -> mapping
(** [create_mapping ~const ~used] creates a mapping from all instances
    of type [t] that are free in set [used] and are either variables or
    constants in a case when [const] parameter is true.
*)
