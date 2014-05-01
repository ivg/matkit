%{
open Ast
open Exp
open Decl
%}

(* punctuation and keywords *)
%token END DOT COMMA LPAR RPAR LCUR RCUR
%token WHERE IS IN RING AND

(* operations *)
%token PLUS MINUS
%token MUL DIV
%token HAD HDIV
%token POW HPOW
%token NEG CONJ INV TRAN
%token EQUALS

(* associativity and precedence *)
%nonassoc EQUALS
%left PLUS MINUS
%left MUL DIV HAD HDIV
%left POW HPOW
%nonassoc INV TRAN CONJ
%nonassoc UNEG

%token <string> NUM
%token <string> SYM

%start script
%type < Ast.script > script;

%%

script: stmts END {$1};

stmts:
  | stmt {[$1]}
  | stmt stmts {$1 :: $2}
  ;

stmt:
  | expr COMMA decls DOT            { (Some $1, $3) }
  | expr DOT                        { (Some $1, [])}
  | decls DOT                       { (None, $1)}
  ;

(*** EXPRESSIONS ***)
expr:
  | term                      { $1 }
  | term expr %prec MUL       { Bop(Mul, $1, $2) }
  | lhs=expr o=binop rhs=expr { Bop(o, lhs, rhs) }
  ;

term:
  | LPAR expr RPAR         { $2 }
  | SYM                    { Var $1 }
  | NUM                    { Num (float_of_string $1) }
  | NEG t=term  %prec UNEG { Uop(UNeg, t) }
  | t=term INV             { Bop(Pow, t, Num (-1.)) }
  | t=term TRAN            { Uop(Tran, t) }
  | t=term CONJ            { Uop(Conj, t) }
  ;

%inline binop:
  | EQUALS          { Eql }
  | PLUS            { Add }
  | MINUS           { Sub }
  | MUL             { Mul }
  | HAD             { Had }
  | POW             { Pow }
  | DIV             { Div }
  | HDIV            { HDiv }
  | HPOW            { HPow }
  ;

(*** DECLARATIONS ***)
(** build up a list of properties and then add to Assoc list **)
decls:
  | SYM IS props                   { Decl.create $1 $3 }
  | WHERE SYM IS props             { Decl.create $2 $4 }
  | SYM IS props COMMA decls       { (Decl.create $1 $3) @ $5 }
  | WHERE SYM IS props COMMA decls { (Decl.create $2 $4) @ $6 }
  ;

props:
  | str                         { [Kind(Sym.concat $1)] }
  | IN ring_desc                { [$2] }
  | IN RING ring_desc           { [$3] }
  | str AND props               { Kind(Sym.concat $1) :: $3 }
  | IN ring_desc AND props      { $2 :: $4 }
  | IN RING ring_desc AND props { $3 :: $5 }
  ;

(* strings are passed as a series of chars and must be reconstructed *)
str:
  | SYM           { [$1] }
  | SYM str       { $1 :: $2 }
  ;

ring_desc:
  | SYM LCUR dims RCUR { Ring(Ring.ring_of_str $1, Some $3) }
  | LCUR dims RCUR     { Ring(Ring.ring_of_str "R", Some $2) }
  | SYM                { Ring(Ring.ring_of_str $1, None) }
  ;

dims:
  | dim           { ($1, INum (Nat1.of_int_exn 1)) }
  | dim COMMA dim { ($1, $3) }
  ;

dim:
  | NUM { INum (Nat1.of_int_exn (int_of_string $1)) }
  | SYM { IVar $1 }
  ;
