open Core.Std
open Ast

type t = ring with sexp,compare,enumerate
include Stringable with type t := t

val real: t
val integer: t
val complex: t


val ppr: t -> ppr
