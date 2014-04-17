open Core.Std
open Ast


type parser_output = (exp option * (char * string) list) list
with sexp

let string_of_parser_output out =
  Sexp.to_string (sexp_of_parser_output out)



let run_test data =
  Parser.script Lexer.tokens (Lexing.from_string data)
  |> string_of_parser_output
  |>printf "%s\n"

let run_tests () =
  run_test "a.";
  run_test "a + b.";
  run_test "a - b.";
  run_test "a * b.";
  run_test "a + b * c.";
  run_test "a * b + c.";
  run_test "~a - b.";
  run_test "~a + ~b - c."

let () = run_tests ()



