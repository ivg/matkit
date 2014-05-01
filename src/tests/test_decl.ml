open Core.Std
open Ast

let () =
  let d1,d2 = Dim.(of_sym "N", of_sym "M") in
  let d1 : decls =
    Decl.(assoc "A" [
        real ~d1 ~d2;
        matrix;
        kind ~is:"symmetric";
        kind ~is:"invertible"
      ] @ assoc "B" [
        is_real; matrix;
        kind ~is:"invertible";
        kind ~is:"square"]) in

  ()
