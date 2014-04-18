open Core.Std
open Ast
open Type


val init: exp option -> (char * string) list -> Env.t * constrs

val recon: Env.t -> exp -> ty * constrs
(** [recon env exp] reconstruct type of expression [exp] and
    generates constraints *)

val infer: (char *string) list -> exp -> ty Sym.Map.t
(** [infer env exp] infer a type of expression  *)

