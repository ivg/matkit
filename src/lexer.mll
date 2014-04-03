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


let lowercase = ['a'-'z']
let newline = '\n' | '\r' | "\r\n" | "\n\r"
let space = ' ' | '\t'

rule tokens = parse
  | eof {END}
  | space {tokens lexbuf}
  | newline {new_line lexbuf; tokens lexbuf}
  | "." {DOT}
  | lowercase as s {SYM s}
  | "(" {LPAR}
  | ")" {RPAR}
  | "#" {comment lexbuf}
and comment = parse
  | newline {new_line lexbuf; tokens lexbuf}
  | eof {END}

{
}

