%{
open Ast
%}

(* keywords *)
%token END DOT COMMA LPAR RPAR WHERE IS IN RING AND

(* operations *)
%token PLUS MINUS
%token MUL DIV
%token HAD HDIV
%token POW HPOW
%token NEG CONJ INV TRAN
%token EQUALS

(* associativity and precedence *)
%nonassoc EQUALS
%left PLUS
%left MINUS
%left MUL
%left HAD

%token <char> SYM
%token <string> KIND
%token <string> NUM

%start script
%type <(Ast.exp option * ring option * kind list) list> script;

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
  | term                      { $1 }
  | expr INV                  { Bop(Pow, $1, Num (-1)) }
  | lhs=expr o=binop rhs=expr { Bop(o, lhs, rhs) }
  | t=term op=post_unop       { Uop(op, t) }
  ;

term:
  | op=pre_unop t=term  { Uop(op, t) }
  | LPAR expr RPAR      { $2 }
  | SYM                 { Exp.var $1 }
  | NUM                 { Num(int_of_string $1) }
  ;

%inline pre_unop:
  | NEG             { UNeg }
  ;

%inline post_unop:
  | TRAN            { Tran }
  | CONJ            { Conj }
  ;

%inline binop:
  | PLUS            { Add }
  | MINUS           { Sub }
  | MUL             { Mul }
  | HAD             { Had }
  | POW             { Pow }
(*  | DIV             { Div } *)
(*  | HDIV            { HDiv } *)
(*  | HPOW            { HPow } *)
  ;

decls:
  | decl                   { [$1] }
  | decl COMMA decls       { $1::$3 }
  | decl COMMA WHERE decls { $1::$4 }
  ;

decl:
  | SYM IS kinds { ($1,$3) }
  | SYM IN rings { ($1,$3) }
  | SYM IN RING rings { ($1,$4) }
  ;

kinds:
  | KIND           { [$1] }
  | KIND AND kinds { $1 :: $3 }
  ;

rings:
  | SYM           { [string_of_char $1] }
  | SYM AND rings { (string_of_char $1) :: $3 }
  ;
