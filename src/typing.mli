open Core.Std
open Ast
open Type


val recon: Env.t -> exp -> ty * constrs
(** [recon env exp] reconstruct type of expresgenerates  *)

val infer: Env.t -> exp -> ty
