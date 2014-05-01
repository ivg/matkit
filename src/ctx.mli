(** Context is a mapping from term to types. *)
open Core.Std
open Ast
open Type

type t with sexp

val empty: unit -> t
(** [empty ()] creates an empty context.  *)

val add: t -> exp -> ty -> unit

val add_fresh: t -> exp -> ty
(** [add_fresh ctx t] creates a fresh type variable and bind it
    with term [t]. Return the freshly created type variable. *)

val get_or_add_fresh: t -> exp -> ty
(** [get_or_add_fresh ctx t] if [t] is bound in [ctx] then return
    its type, else create a fresh variable, bound it to [id] and
    return it.  *)

val find: t -> exp -> ty option
(** [find env t] performs a lookup in the context.  *)

val is_bound: t -> exp -> bool
(** [is_bound env t] true if term [t] is bound in context [env]. *)

val bound_dims: t -> Dim.Set.t

val create_substitution: t -> UnionFind.t -> subst
(** [create_substitution ctx unifier] accepts a unifier and
    creates a mapping from terms to types.  *)
