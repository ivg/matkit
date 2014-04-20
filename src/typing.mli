open Core.Std
open Ast
open Type


val infer: (char *string) list -> exp -> subst
(** [infer env exp] infer a type of expression  *)

