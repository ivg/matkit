open Core.Std
open Ast
open Type

val infer: Env.t -> exp -> ty
