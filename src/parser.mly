%{
open Ast
%}

%token END DOT LPAR RPAR IS WHERE COMMA
%token <char> SYM
%token <string> KIND

%start script
%type <(Ast.exp option * (char * string) list) list> script;

%%

script: stmts END {$1};

stmts:
  | stmt {[$1]}
  | stmt stmts {$1 :: $2}
  ;

stmt:
  | expr COMMA WHERE decls DOT { (Some $1,$4) }
  | expr WHERE decls DOT       { (Some $1,$3)}
  | expr DOT                   { (Some $1,[])}
  | decls DOT                  { (None,$1)}
  ;

expr:
  | LPAR expr RPAR {$2}
  | SYM {Exp.var $1}
  ;

decls:
  | decl {[$1]}
  | decl COMMA decls { $1 :: $3}
  ;

decl:
  | SYM IS KIND {($1,$3)}
  ;



