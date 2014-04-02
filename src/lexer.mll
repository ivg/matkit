{
open Core.Std
open Lexing
open Parser
}

rule tokens = parse
  | eof {END}
and comment = parse
  | eof {END}

{
}

