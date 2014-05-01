open Core.Std
open Ast


let () =
  let d1,d2 = Dim.(of_sym "N", of_sym "M") in
  let d1 : decls =
    Decl.(assoc "A" [
        real ~d1 ~d2;
        kind ~is:"symmetric";
        kind ~is:"invertible"
      ] @ assoc "B" [
        is_real;
        kind ~is:"invertible";
        kind ~is:"square"]) in


  let s1 = Printer.show Decl.ppr_list d1 in

  printf "s1 = %s\n" s1;

  ()
