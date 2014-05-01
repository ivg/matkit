(** Declarations. *)


(** {3 Examples}

    [(decl "A"
          [ring "r" Some(IVar "m",IVar "n");
           kind "symmetric";
           kind "invertible"]) @
    (decl "B" [ring "r" None; kind "invertible"; kind "square"])]

*)


open Core.Std
open Ast
open Type


type t = decls with sexp,compare

val create: sym -> property list -> t
(** [create sym props] creates a declaration give a set of properties
    [props] assosiated with a symbol [sym] *)

val ring: sym -> ty option -> property
val kind: sym -> property
