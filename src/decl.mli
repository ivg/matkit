open Core.Std
open Ast

type t = decls

type dim =
  | Const_dim of int
  | Var_dim of string
  | Rigid_dim of string
  | Empty
with sexp, compare

type dims = dim * dim with sexp,compare

val kind_list_of_props sym -> property list -> t

val ring: sym -> ?dims -> property 
val kind: sym -> property
val decl: sym -> property list -> t

(*
creating decls

(decl "A" [ring "r" (Var_dim "m",Var_dim "n"); 
           kind "symmetric";
           kind "invertible"]) @
(decl "B" [ring "r"; kind "invertible; kind "square"])

*)
