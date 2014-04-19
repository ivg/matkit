open Core.Std
open Ast


type parser_output = (exp option * (char * string) list) list
with sexp

let string_of_parser_output out =
  Sexp.to_string (sexp_of_parser_output out)


let test_data (data: string list) =
  List.iter ~f:(fun x ->
    Parser.script Lexer.tokens (Lexing.from_string x)
    |> string_of_parser_output
    |>printf "%s\n") data
    
let test_term () =
  printf "*** Testing Single Terms ***\n";
  test_data [
    "1.";
    "a.";
    "(1).";
    "(a).";
  ]
  
let test_unop () =
  printf "*** Testing Unary Operations ***\n";
  test_data [
    "~1.";  
    "-1.";
    "~a.";
    "~(1).";
    "~(a).";
    "a`.";
    "~a`.";
    "(~a)`.";
    "~~a``.";
    "a'."
  ]

let test_binop () =
  printf "*** Testing Binary Operations ***\n";
  test_data [
    "a + b.";
    "a - b.";
    "a + b - c.";
    "a * b.";
    "a .* b.";
    "a * b + c.";
    "(a + b) .* c.";
    "~a * b.";
    "~(a - b) * c.";
    "~(4 * a) - (b + c)."
  ]

let run_tests () =
  test_term ();
  test_unop ();
  test_binop ()

let () = run_tests ()



