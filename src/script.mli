open Core.Std
open Ast

type t = script with sexp,compare

val load: In_channel.t -> t
val save: t -> Out_channel.t -> unit
