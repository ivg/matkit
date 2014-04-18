open Core.Std
open Ast
open Type


val init: Ast.t option -> (char * string) list -> Env.t * constrs

val recon: Env.t -> exp -> ty * constrs
(** [recon env exp] reconstruct type of expression [exp] and
    generates constraints *)

val infer: (char *string) list -> exp -> ty
(** [infer env exp] infer a type of expression  *)

