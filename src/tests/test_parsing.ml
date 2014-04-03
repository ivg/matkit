open Core.Std


type parser_output = (Ast.t option * (char * string) list) list with sexp

let run_test () =
  let data = "a." in
  let out = Parser.script Lexer.tokens (Lexing.from_string data) in
  printf "%s" (Sexp.to_string (sexp_of_parser_output out))

let () = run_test ()



