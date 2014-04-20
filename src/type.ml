(** Type definitions for a typing phase.

    Type of expression is defined as pair of two terms, where each
    term specifies an input and output dimension of a matrix.

    Dimension term is either a variable or a constant. No higher order
    terms yet.
  *)

open Core.Std
open Ast

(** type of an expression. A special kind of term, consisting only
    from varialbles.  *)
type ty = index * index
with sexp,compare

module Type =
  Comparable.Make(struct type t = ty with sexp,compare end)

(** [i1,i2] constrains dimension [i1] to be equal to [i2] *)
type constr = index * index
with sexp,compare

(** list of constraints  *)
type constrs = constr list with sexp,compare

(** a mapping from expressions to types  *)
type subst = ty Exp.Map.t
