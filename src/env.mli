(** Context is a set of bindings from symbols to types  *)
open Ast
open Type

type t with sexp

val empty: unit -> t

val add_fresh: t -> exp -> ty
(** [add_fresh ctx sym] creates a fresh type variable and bind it
    with identifier [sym]. Return the freshly created variable *)

val get_or_add_fresh: t -> exp -> ty
(** [get_or_add_fresh ctx id] if [id] is bound in [ctx] then return
    its type, else create a fresh variable, bound it to [id] and
    return it  *)

val find: t -> exp -> ty option
val is_bound: t -> exp -> bool
