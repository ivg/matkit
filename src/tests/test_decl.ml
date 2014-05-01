open Core.Std
open Ast

let () = 
  printf "*** Testing Simple Decls ***\n";
  assert(Decl.empty = []);
  let decl = Decl.decl "A" [Decl.ring "r" (Some(IVar "m", IVar "n")); 
                            Decl.kind "invertible"] in
  assert(decl = [("A", Ring(Ring.ring_of_str "r", Some(IVar "m", IVar "n")));
                 ("A", Kind("invertible"))])
