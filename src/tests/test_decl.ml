open Core.Std
open Ast

let _ = Decl.decl "A" [Decl.ring "r" (Some(IVar "m", IVar "n")); 
                       Decl.kind "invertible"]
