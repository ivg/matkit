(** Context is a set of bindings from symbols to types  *)
open Ast
open Type

type t

val empty: unit -> t

val add_fresh: t -> sym -> ty
(** [add_fresh ctx sym] creates a fresh type variable and bind it
    with identifier [sym]. Return the freshly created variable *)

val get_or_add_fresh: t -> sym -> ty
(** [get_or_add_fresh ctx id] if [id] is bound in [ctx] then return
    its type, else create a fresh variable, bound it to [id] and
    return it  *)
