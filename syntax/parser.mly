%{
  open Ast
%}

%token <int>           INT
%token <string>        STR
%token <Symbol.symbol> ID
%token                 EOF

%start <Ast.exp> prog

%%

prog:
    | e = expr EOF
      { e }

expr:
    | i = INT
      { IntExp i }
    | s = STR
      { StringExp (s, to_pos($startpos)) }
