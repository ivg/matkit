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

(* id *)
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let character = ['a'-'z' 'A'-'Z']

(* numbers *)
let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let integer = '-'? digit+
let decimal = digit* frac? exp?

(* whitespace *)
let newline = '\n' | '\r' | "\r\n" | "\n\r"
let space = ' ' | '\t'
let whitespace = (newline | space)*

(* keywords *)
let where = whitespace* " where " whitespace+
let is = whitespace* " is " whitespace*
let in_t = whitespace* " in " whitespace*
let ring = whitespace* " ring " whitespace*
let and_t = whitespace* " and " whitespace*

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
  | character as s {SYM s}
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

