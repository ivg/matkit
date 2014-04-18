open Core.Std
open Ast

type t

val create: unit -> t
val find: t -> index -> index
val union: t -> index -> index -> t
