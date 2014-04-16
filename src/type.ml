(** Type definitions for a typing phase  *)

open Core.Std
open Ast

type ty =
  | TVar of sym                     (** type variable  *)
  | TNum                            (** scalar  *)
  | TMat of index * index           (** matrix  *)
with sexp,compare,variants


module Type =
  Comparable.Make(struct type t = ty with sexp,compare end)

module Constr = struct
  type t =
    | Equal of ty * ty
    | LComm of ty * ty
    | RComm of ty * ty
  with sexp,compare,variants
end

type constrs = Constr.t list with sexp,compare
