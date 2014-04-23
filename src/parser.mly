%{
open Ast
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
  | NUM                 { Num (int_of_string $1) }
  ;

%inline pre_unop:
  | NEG             { UNeg }
  ;

%inline post_unop:
  | TRAN            { Tran }
  | CONJ            { Conj }
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
  | syms           { [concat_char_list $1] }
  | syms AND kinds { (concat_char_list $1) :: $3 }
  ;
  
syms:
  | SYM            { [$1] }
  | SYM syms       { $1 :: $2 }
  
ring:
  | SYM IN ring_desc      { $3 }
  | SYM IN RING ring_desc { $4 }

ring_desc:  
  | SYM LCUR dims RCUR { (Ring.ring_of_char $1, Some $3) }
  | LCUR dims RCUR     { (Ring.ring_of_char 'R', Some $2) }
  | SYM                { (Ring.ring_of_char $1, None) }
  
dims:
  | dim COMMA dim { ($1, $3) }
  | dim           { ($1, INum (Nat1.of_int_exn 1)) }
  
dim:
  | NUM { INum (Nat1.of_int_exn (int_of_string $1)) }
  | SYM { IVar (string_of_char $1) }
   

