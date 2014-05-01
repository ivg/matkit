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
  |>printf "%s\n"


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
    ((Some(Exp.(Var "A")), Decl.empty), "A.");
    ((Some(Exp.(Var "a")), Decl.empty), "a.");
    ((Some(Exp.(neg (Num 1.))), Decl.empty), "~1.");
    ((Some(Exp.(Var "A")), Decl.empty), "(A).")
  ]

let test_unop () =
  printf "*** Testing Unary Operations ***\n";
  test_parses [
    ((Some(Exp.(neg (Var "A"))), Decl.empty), "~A.");
    ((Some(Exp.(tran (Var "A"))), Decl.empty), "A'.");
    ((Some(Exp.(tran (neg (Var "A")))), Decl.empty), "~A'.");
    ((Some(Exp.(neg (tran (Var "A")))), Decl.empty), "~(A').")
  ]

let test_binop () =
  printf "*** Testing Binary Operations ***\n";
  test_parses [
    ((Some(Exp.(Var "A" + Var "B")), Decl.empty), "A+B.");
    ((Some(Exp.(Var "A" - Var "B")), Decl.empty), "A-B.");
    ((Some(Exp.(Var "A" * Var "B")), Decl.empty), "A*B.");
    ((Some(Exp.(Var "A" /. Var "B")), Decl.empty), "A./B.");
    ((Some(Exp.(Var "A" *. Var "B")), Decl.empty), "A.*B.");
    ((Some(Exp.(Var "A" ** Num 2.)), Decl.empty), "A^2.");
    ((Some(Exp.(Var "A" **. Var "B")), Decl.empty), "A.^B.");
    ((Some(Exp.(Var "A" + (Var "B" * Var "C"))), Decl.empty), "A+B*C.");
    ((Some(Exp.(Var "A" * Var "B" + Var "C")), Decl.empty), "A*B+C.");
    ((Some(Exp.(Var "A" + (Var "B" * (Var "C" ** Var "D")))), Decl.empty), "A+B*C^D.");
    ((Some(Exp.(Var "A" * Var "B")), Decl.empty), "AB.");
    ((Some(Exp.(neg (Var "A") * (neg (Var "B")))), Decl.empty), "~A~B.");
    ((Some(Exp.(tran (neg (Var "A")) * Var "B")), Decl.empty), "~A'B.")
  ]

let test_combinations () =
  printf "*** Testing Combined Operations ***\n";
  test_parses [
    ((Some(Exp.(neg (Var "A[i,j]") + Var "B")), Decl.empty), "~A[i,j]+B.");
    ((Some(Exp.(neg (Var "A") + (neg (Var "B")))), Decl.empty), "~A+~B.");
    ((Some(Exp.(tran (neg (Var "A")) * (tran (neg (Var "B"))))), Decl.empty), "~A'*~B'.");
    ((Some(Exp.(((neg (Num 4.)*(tran(neg (Var "A"))))+(tran(Var "B")))-
                 ((neg (Var "C"))*(Num 4.)))), Decl.empty), "~4*~A'+B'-~C*4.");
    ((Some(Exp.(tran (neg (tran (neg (Var "A" + Var "B")))))), Decl.empty), "~(~(A+B)')'.")
  ]

let test_decls () =
  printf "*** Testing Decls ***\n";
  test_parses [
    ((None, Decl.(decl "A" [kind "invertible"])), "A is invertible.");
    ((None, Decl.(decl "A" [ring "r" (Some(IVar "m", IVar "n"))])), "A is in r {m,n}.");
    ((None, Decl.(decl "A" [kind "invertible";ring "r" (Some(IVar "m", IVar "n"))])),
      "A is invertible and in ring {m,n}.");
    ((None, Decl.(decl "A" [kind "square";kind "invertible"; ring "r" None])),
      "A is square and invertible, A is in ring r.");
    ((None, Decl.(decl "A" [ring "r" (Some(IVar "m", INum Nat1.one)); kind "invertible"])),
      "A is in r {m} and invertible.");
    ((None, (Decl.(decl "A" [kind "invertible"])) @ (Decl.(decl "B" [kind "invertible"]))),
      "A is invertible,
       B is invertible.");
    ((None, (Decl.(decl "A" [kind "invertible"])) @ (Decl.(decl "B" [kind "invertible"])) @
            (Decl.(decl "A" [ring "r" None]))),
      "A is invertible,
       B is invertible,
       A is in r.");

  ]

let run_tests () =
  test_term ();
  test_unop ();
  test_binop ();
  test_combinations ();
  test_decls ()

let () = run_tests ()



