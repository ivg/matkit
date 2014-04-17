(** Type definitions for a typing phase  *)

open Core.Std
open Ast

type ty = index * index
with sexp,compare

module Type =
  Comparable.Make(struct type t = ty with sexp,compare end)

type constr = index * index
with sexp,compare

type constrs = constr list with sexp,compare
