(** An implementation of infer subcommand.  *)

open Core.Std
open Ast

val process: script -> script
(** [process script] returns a script with a set of declarations
    extended by infered types.  *)
