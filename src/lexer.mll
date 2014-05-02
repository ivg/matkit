{
open Core.Std
open Lexing
open Parser
}

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
(** indexing not implemented **)
(*let index = '[' (integer | character+) (',' (integer | character+))? ']'*)
let subscript = '_' character
let id = character subscript?

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
  | "\"" {CONJ}
  | "#" {comment lexbuf}
and comment = parse
  | newline {new_line lexbuf; tokens lexbuf}
  | _ {comment lexbuf}
  | eof {END}

{
}
