(** Type definitions for a typing phase.

    Type is a term. Term is either a type variable, constant or a pair
    of terms [t1,t2], denoting a transformation from t2 to t1. But,
    since all operations in our algebra are closed over matrices
    (i.e., any operation on matrix returns a matrix) we can remove
    reduntant variants and represent type as a pair of indices.

  *)

open Core.Std
open Ast

(** type of an expression.  *)
type ty = dim * dim
with sexp,compare

module Type =
  Comparable.Make(struct type t = ty with sexp,compare end)

(** [i1,i2] constrains dimension [i1] to be equal to [i2] *)
type constr = dim * dim
with sexp,compare

(** list of constraints  *)
type constrs = constr list with sexp,compare

(** a mapping from expressions to types  *)
type subst = ty Exp.Map.t
