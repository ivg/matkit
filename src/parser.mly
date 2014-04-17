%{
open Ast
%}

(* keywords *)
%token END DOT COMMA LPAR RPAR IS WHERE AND

(* operations *)
%token PLUS MINUS
%token MUL DIV
%token HAD HDIV
%token POW HPOW
%token NEG CONJ
%token EQUALS

(* associativity and precedence *)
%left PLUS
%left MINUS
%left MUL
%left NEG

%token <char> SYM
%token <string> KIND
%token <string> NUM

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
  | term            { $1 }
  | expr binop expr { Bop($2, $1, $3) }
  | unop term       { Uop($1, $2) }  
  ;
  
term:
  | LPAR expr RPAR  { $2 }
  | SYM             { Exp.var $1 }
  | NUM             { Num(int_of_string $1) }
  ;
  
unop:
  | NEG             { UNeg }
  | CONJ            { Conj }
  ;
  
binop:
  | PLUS            { Add }
  | MINUS           { Sub }
  | MUL             { Mul }
  ;

decls:
  | decl {[$1]}
  | decl COMMA decls { $1::$3 }
  ;

decl:
  | SYM IS KIND { ($1,$3) }
  ;



