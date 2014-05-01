open Core.Std
open Ast

let _ = Decl.(create "A" [ring "r" (Some(IVar "m", IVar "n"));
                          kind "invertible"])
