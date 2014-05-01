open Core.Std
open Ast

type t = decls
type d = dims

val kind_list_of_props: sym -> property list -> t

val empty: t
val to_dim: int -> dim
val ring: sym -> dims option -> property 
val kind: sym -> property
val decl: sym -> property list -> t
val to_string: t -> string


(*
creating decls

(decl "A" [ring "r" Some(IVar "m",IVar "n"); 
           kind "symmetric";
           kind "invertible"]) @
(decl "B" [ring "r" None; kind "invertible"; kind "square"])

*)
