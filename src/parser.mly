%{
open Ast
%}

%token END

%start script
%type <Env.t * Ast.t list> script;

%%

script:
  | END {Env.empty (), [] }
  ;


