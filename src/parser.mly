%{
open Ast
open Exp
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
%token <char> SYM

%start script
%type <(Ast.exp option * Ast.ring option * Ast.kind list) list> script;

%%

script: stmts END {$1};

stmts:
  | stmt {[$1]}
  | stmt stmts {$1 :: $2}
  ;

stmt:
  | expr COMMA ring COMMA decls DOT { (Some $1, Some $3, $5) }
  | expr COMMA decls DOT            { (Some $1, None, $3) }
  | expr COMMA ring DOT             { (Some $1, Some $3, [] ) }
  | expr DOT                        { (Some $1, None, [])}
  | decls DOT                       { (None, None, $1)}
  ;

(*** EXPRESSIONS ***)
expr:
  | term                      { $1 }
  | term expr %prec MUL       { Bop(Mul, $1, $2) }
  | lhs=expr o=binop rhs=expr { Bop(o, lhs, rhs) }
  ;

term:
  | LPAR expr RPAR         { $2 }
  | SYM                    { Exp.var $1 }
  | NUM                    { Num (int_of_string $1) }
  | NEG t=term  %prec UNEG { Uop(UNeg, t) }
  | t=term INV             { Bop(Pow, t, Num (-1)) }
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
decls:
  | decl                   { $1 }
  | WHERE decl             { $2 }
  | decl COMMA decls       { $1@$3 }
  | WHERE decl COMMA decls { $2@$4 }
  ;

decl:
  | SYM IS kinds { kind_list_of_strings $1 $3 }
  ;

kinds:
  | str           { [concat_char_list $1] }
  | str AND kinds { (concat_char_list $1) :: $3 }
  ;

(* strings are passed as a series of chars and must be reconstructed *)
str:
  | SYM           { [$1] }
  | SYM str       { $1 :: $2 }
  ;

(*** RINGS ***)
ring:
  | SYM IN ring_desc               { $3 }
  | SYM IN RING ring_desc          { $4 }
  | WHERE SYM IS IN ring_desc      { $5 }
  | WHERE SYM IS IN RING ring_desc { $6 }
  ;

ring_desc:
  | SYM LCUR dims RCUR { (Ring.ring_of_char $1, Some $3) }
  | LCUR dims RCUR     { (Ring.ring_of_char 'R', Some $2) }
  | SYM                { (Ring.ring_of_char $1, None) }
  ;

dims:
  | dim           { ($1, INum (Nat1.of_int_exn 1)) }
  | dim COMMA dim { ($1, $3) }
  ;

dim:
  | NUM { INum (Nat1.of_int_exn (int_of_string $1)) }
  | SYM { IVar (string_of_char $1) }
  ;

