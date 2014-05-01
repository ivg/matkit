open Core.Std
open Ast
open Type


val infer: script -> subst
(** [infer env exp] infer a type of expression  *)

val token_of_subst: subst -> Treeprint.Printer.token
