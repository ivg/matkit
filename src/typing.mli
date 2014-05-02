open Core.Std
open Ast
open Type


val infer: script -> subst
(** [infer env exp] infer a type of expression  *)


val is_scalar: ty -> bool
val is_vector: ty -> bool
