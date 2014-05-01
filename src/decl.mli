(** This module handles user declarations. A declaration is a pair of
    build of a symbol and a property. See Ast module for a definitions.
*)


(** {3 Examples}

    [matrix "A"] states that "A" is a matrix.

    a more general form [kind] accepts an arbitrary string:
    [kind "A" ~is:"symmetic"]

    Assoc combines several declarations for one symbol:

    [let d1,d2 = Dim.(of_sym "N", of_sym "M") in
     let d1 : decls =
     Decl.(assoc "A" [
         real ~d1 ~d2;
         matrix;
         kind ~is:"symmetric";
         kind ~is:"invertible"
       ] @ assoc "B" [
         is_real; matrix;
         kind ~is:"invertible";
         kind ~is:"square"])]
*)


open Core.Std
open Ast
open Type

type t = decl with sexp,compare

val is_real: sym -> t
val is_integer: sym -> t
val is_complex: sym -> t


type ring_decl = d1:Dim.t -> d2:Dim.t -> t

val ring: sym -> r:Ring.t -> ring_decl
(** [ring sym ~r ~d1 ~d2] creates a ring declaration *)

(** shortcuts for rings: *)
val real: sym -> ring_decl
val integer: sym -> ring_decl
val complex: sym -> ring_decl


val kind: sym -> is:sym -> t
(** [kind sym ~is:something] creates a kind defined by an arbitrary
    symbol [something].   *)

val matrix: sym -> t
(** [matrix sym] declares [sym] to be a matrix  *)
val vector: sym -> t
(** [vector sym] declares [sym] to be a vector  *)
val scalar: sym -> t
(** [scalar sym] declares [sym] to be a scalar  *)

val assoc: sym -> (sym -> t) list -> t list
(** [assoc sym props] assosiates properties [props] with a symbol
    [sym] *)

val ppr: t -> ppr

val ppr_list: t list -> ppr
