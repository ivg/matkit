{
open Core.Std
open Lexing
open Parser

(* inside this curly brackets you can write any helper functions in a
   pure ocaml. Though, I'll suggest to put your code in a separate module.

   The code here will be called before lexer invocation.
*)


}


(* here you can define shortcuts for regexps in a simple syntactic
   form «let name = regexp». *)



(* numbers *)
let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let integer = digit+
let decimal = digit* frac? exp?

(* id *)
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let character = (lowercase | uppercase)
let index = '[' (integer | character+) (',' (integer | character+))? ']'
let id = character index? 

(* whitespace *)
let newline = '\n' | '\r' | "\r\n" | "\n\r"
let space = ' ' | '\t'
let whitespace = (newline | space)*

(* keywords *)
(* current definition is not sufficient, reimplement
 * after word break sign is worked out *)
let where = "where "
let is = "is " 
let in_t = "in "
let ring = "ring " 
let and_t = "and " 

rule tokens = parse
  | eof {END}
  | space {tokens lexbuf}
  | newline {new_line lexbuf; tokens lexbuf}
  | "." {DOT}
  | where {WHERE}
  | is    {IS}
  | in_t  {IN}
  | ring  {RING}
  | and_t {AND}
  | id as s {SYM s}
  | integer as i {NUM i}
  | "=" {EQUALS}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {MUL}
  | "/" {DIV}
  | ".*" {HAD}
  | "./" {HDIV}
  | "^" {POW}
  | ".^" {HPOW}
  | "~" {NEG}
  | "`" {INV}
  | "\'" {TRAN}
  | "(" {LPAR}
  | ")" {RPAR}
  | "{" {LCUR}
  | "}" {RCUR}
  | "," {COMMA}
  | "#" {comment lexbuf}
and comment = parse
  | newline {new_line lexbuf; tokens lexbuf}
  | eof {END}

{
}

