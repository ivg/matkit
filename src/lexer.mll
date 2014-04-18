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
let kind = lowercase+

(* numbers *)
let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let integer = '-'? digit+
let decimal = digit* frac? exp?

(* whitespace *)
let newline = '\n' | '\r' | "\r\n" | "\n\r"
let space = ' ' | '\t'

let apostrophe = '\''

rule tokens = parse
  | eof {END}
  | space {tokens lexbuf}
  | newline {new_line lexbuf; tokens lexbuf}
  | "." {DOT}
  | lowercase as s {SYM s}
  | uppercase as s {SYM s}
  | integer as i {NUM i}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {MUL}
  | "/" {DIV}
  | ".*" {HAD}
  | "^" {POW}
  | ".^" {HPOW}
  | "~" {NEG}
  | "`" {INV}
  | "\'" {TRAN}
  | "(" {LPAR}
  | ")" {RPAR}
  | "where" {WHERE}
  | "is"    {IS}
  | "and"   {AND}
  | "#" {comment lexbuf}
and comment = parse
  | newline {new_line lexbuf; tokens lexbuf}
  | eof {END}

{
}

