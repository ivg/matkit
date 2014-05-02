(** A Lexer.
    Tokenizes an input stream of chars.
*)


val tokens: Lexing.lexbuf -> Matlan_parser.token
(** [tokens buf] returns a token from a lexing buffer [buf]  *)
