open Core.Std
open Sexplib.Std
open Ast

type parser_output = script
with sexp

(*** PARSE, EXTRACT, PRINT AST ***)
let string_of_parser_output out =
  Sexp.to_string (sexp_of_parser_output out)

let parse x = Parser.script Lexer.tokens (Lexing.from_string x)

let extract_exp (x: parser_output)  =
  match x with
  | [] | (None, _) :: _ -> failwith "No representation for empty exp\n"
  | (Some e, _) :: _ -> e

let print_parse data =
  parse data
  |> string_of_parser_output
  |>printf "%s\n"


(*** TEST PARSING HELPERS ***)
let assert_parse e str =
  assert(e = extract_exp (parse str))

let test_parse e str =
  print_parse str;
  assert_parse e str

let test_parses (input: (exp * string) list) =
  List.iter ~f:(fun (e,s) -> test_parse e s) input


(*** TESTING FUNCTIONS ***)
let test_term () =
  printf "*** Testing Single Terms ***\n";
  test_parses [
    (Exp.(Var "A"), "A.");
    (Exp.(Var "a"), "a.");
    (Exp.(neg (Num 1.)), "~1.");
    (Exp.(Var "A"), "(A).")
  ]

let test_unop () =
  printf "*** Testing Unary Operations ***\n";
  test_parses [
    (Exp.(neg (Var "A")), "~A.");
    (Exp.(tran (Var "A")), "A'.");
    (Exp.(tran (neg (Var "A"))), "~A'.");
    (Exp.(neg (tran (Var "A"))), "~(A').")
  ]

let test_binop () =
  printf "*** Testing Binary Operations ***\n";
  print_parse "ABC + ABC.";
  test_parses [
    (Exp.(Var "A" + Var "B"), "A+B.");
    (Exp.(Var "A" - Var "B"), "A-B.");
    (Exp.(Var "A" * Var "B"), "A*B.");
    (Exp.(Var "A" /. Var "B"), "A./B.");
    (Exp.(Var "A" *. Var "B"), "A.*B.");
    (Exp.(Var "A" ** Num 2.), "A^2.");
    (Exp.(Var "A" **. Var "B"), "A.^B.");
    (Exp.(Var "A" + (Var "B" * Var "C")), "A+B*C.");
    (Exp.(Var "A" * Var "B" + Var "C"),  "A*B+C.");
    (Exp.(Var "A" + (Var "B" * (Var "C" ** Var "D"))), "A+B*C^D.");
    (Exp.(Var "A" * Var "B"), "AB.");
    (Exp.(neg (Var "A") * (neg (Var "B"))), "~A~B.");
    (Exp.(tran (neg (Var "A")) * Var "B"), "~A'B.")
  ];
  print_parse "(A^2)C-A."

let test_combinations () =
  printf "*** Testing Combined Operations ***\n";
  test_parses [
    (Exp.(neg (Var "A[i,j]") + Var "B"), "~A[i,j]+B.");
    (Exp.(neg (Var "A") + (neg (Var "B"))), "~A+~B.");
    (Exp.(tran (neg (Var "A")) * (tran (neg (Var "B")))), "~A'*~B'.");
    (Exp.(((neg (Num 4.)*(tran(neg (Var "A"))))+(tran(Var "B")))-((neg (Var "C"))*(Num 4.))),
      "~4*~A'+B'-~C*4.");
    (Exp.(tran (neg (tran (neg (Var "A" + Var "B"))))), "~(~(A+B)')'.")
  ]

let test_kinds () =
  printf "*** Testing Kinds ***\n";
  print_parse "A is square.";
  print_parse "A, where A is square.";
  print_parse "A*x+b, where A is square and invertible.";
  print_parse "A*x+b, where A is in ring R {m}.";
  print_parse "A^4, A is in R {m, n},
                    A is invertible,
                    where A is square and symmetric."

let run_tests () =
  test_term ();
  test_unop ();
  test_binop ();
  test_combinations ();
  test_kinds ()

let () = run_tests ()



