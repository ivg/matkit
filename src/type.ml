(** Type definitions for a typing phase  *)

open Core.Std
open Ast

type ty = index * index
with sexp,compare

module Type =
  Comparable.Make(struct type t = ty with sexp,compare end)

type side =
  | Lhs
  | Rhs
with sexp,compare

type constr =
  | Eq of ty * side * index
  | Commute of ty * side * ty * side
with sexp,compare

type constrs = constr list with sexp,compare
