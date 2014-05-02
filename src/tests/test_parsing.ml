open Core.Std
open Sexplib.Std
open Ast

type parser_output = script
with sexp

(*** PARSE, EXTRACT, PRINT AST ***)
let string_of_parser_output out =
  Sexp.to_string (sexp_of_parser_output out)

let parse x = Parser.script Lexer.tokens (Lexing.from_string x)

let print_parse data =
  parse data
  |> string_of_parser_output
  |> printf "%s%!\n"


(*** TEST PARSING HELPERS ***)

let assert_parse scrpt str =
  let parsed = parse str in
  assert(scrpt = parsed)

let test_parse scrpt str =
  print_parse str;
  assert_parse scrpt str

let test_parses (input: (stmt * string) list) =
  List.iter ~f:(fun (answer,str) -> test_parse [answer] str) input

(*** TESTING FUNCTIONS ***)
let test_term () =
  printf "*** Testing Single Terms ***\n";
  test_parses [
    ((Some(Exp.(Var "A")), []), "A.");
    ((Some(Exp.(Var "a")), []), "a.");
    ((Some(Exp.(neg (Num 1.))), []), "~1.");
    ((Some(Exp.(Var "A")), []), "(A).")
  ]

let test_unop () =
  printf "*** Testing Unary Operations ***\n";
  test_parses [
    ((Some(Exp.(neg (Var "A"))), []), "~A.");
    ((Some(Exp.(tran (Var "A"))), []), "A'.");
    ((Some(Exp.(tran (neg (Var "A")))), []), "~A'.");
    ((Some(Exp.(neg (tran (Var "A")))), []), "~(A').")
  ]

let test_binop () =
  printf "*** Testing Binary Operations ***\n";
  test_parses [
    ((Some(Exp.(Var "A" + Var "B")), []), "A+B.");
    ((Some(Exp.(Var "A" - Var "B")), []), "A-B.");
    ((Some(Exp.(Var "A" * Var "B")), []), "A*B.");
    ((Some(Exp.(Var "A" /. Var "B")), []), "A./B.");
    ((Some(Exp.(Var "A" *. Var "B")), []), "A.*B.");
    ((Some(Exp.(Var "A" ** Num 2.)), []), "A^2.");
    ((Some(Exp.(Var "A" **. Var "B")), []), "A.^B.");
    ((Some(Exp.(Var "A" + (Var "B" * Var "C"))), []), "A+B*C.");
    ((Some(Exp.(Var "A" * Var "B" + Var "C")), []), "A*B+C.");
    ((Some(Exp.(Var "A" + (Var "B" * (Var "C" ** Var "D")))), []), "A+B*C^D.");
    ((Some(Exp.(Var "A" * Var "B")), []), "AB.");
    ((Some(Exp.(neg (Var "A") * (neg (Var "B")))), []), "~A~B.");
    ((Some(Exp.(tran (neg (Var "A")) * Var "B")), []), "~A'B.")
  ]

let test_combinations () =
  printf "*** Testing Combined Operations ***\n";
  test_parses [
    ((Some(Exp.(neg (Var "A_k") + Var "B")), []), "~A_k+B.");
    ((Some(Exp.(neg (Var "A") + (neg (Var "B")))), []), "~A+~B.");
    ((Some(Exp.(tran (neg (Var "A")) * (tran (neg (Var "B"))))), []), "~A'*~B'.");
    ((Some(Exp.(((neg (Num 4.)*(tran(neg (Var "A"))))+(tran(Var "B")))-
                 ((neg (Var "C_123"))*(Num 4.)))), []), "~4*~A'+B'-~C_123*4.");
    ((Some(Exp.(tran (neg (tran (neg (Var "A" + Var "B")))))), []), "~(~(A+B)')'.")
  ]

let test_decls () =
  printf "*** Testing Decls ***\n";
  let d1,d2 = Dim.(of_sym "m", of_sym "n") in

  test_parses [
    ((None, Decl.([kind "A" ~is:"invertible"])), "let A be invertible.");
    ((None, Decl.(assoc "A" [real ~d1 ~d2])), "let A is in R {m,n}.");
    ((None, Decl.(assoc "A" [kind ~is:"invertible"; real ~d1 ~d2])),
      "let A is invertible and in ring {m,n}.");
    ((None, Decl.(assoc "A" [kind ~is:"square"; kind ~is:"invertible"; is_real])),
      "let A is square and invertible, let A be in ring R.");

    ((None, Decl.(assoc "A" [real ~d1 ~d2:Dim.one; kind ~is:"invertible"])),
      "let A is in R {m} and invertible.");

    ((None, (Decl.([kind "A" ~is:"invertible"; kind "B" ~is:"invertible"]))),
      "let A is invertible,
       let B is invertible.");
    ((None, (Decl.([kind "A" ~is:"invertible"; kind "B"
                      ~is:"invertible"] @ assoc "A" [is_real]))),
      "let A be invertible,
       let B be invertible,
       let A be in R.");
    ((None, (Decl.(group_assoc ["A";"B"] [kind ~is:"invertible"; real ~d1 ~d2]))),
      "let A,B be invertible and in ring R {m,n}.");
    ((None, (Decl.(group_assoc ["A";"B"] [kind ~is:"square"; kind ~is:"invertible"]))),
      "let A and B be square and invertible");
  ]

let run_tests () =
  test_term ();
  test_unop ();
  test_binop ();
  test_combinations ();
  test_decls ()

let () = run_tests ()
