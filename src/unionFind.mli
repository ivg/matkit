(** A disjoint-set data structure.  *)
open Core.Std
open Ast

type t

val create: unit -> t
(** [create ()] creates a new disjoint-set  *)

val find: t -> index -> index
(** [find uf t] returns an term, representing  term [t] *)

val union: t -> index -> index -> t
(** [union uf t1 t2] postulates that [t1] and [t2] belongs to the
    same equality class  *)
