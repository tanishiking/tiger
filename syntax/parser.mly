%{
  open Ast
%}

%token <int>           INT
%token <string>        STR
%token <Symbol.symbol> ID
%token                 EOF
%token NIL
%token DOT
%token ASSIGN
%token LBRACKET RBRACKET

%start <Ast.exp> prog

%%

prog:
    | e = expr EOF
      { e }

expr:
    | i = INT
      { IntExp i }
    | NIL { NilExp }
    | s = STR
      { StringExp (s, to_pos($startpos)) }
    | v = var
      { VarExp v }
    | lvalue=var ASSIGN e=expr
      { AssignExp {var=lvalue; exp=e; pos=to_pos($startpos)}}

var:
 | v=ID  { SimpleVar (v, to_pos($startpos)) }
 | v=var DOT f=ID { FieldVar (v, f, to_pos($startpos)) }
 | v=var LBRACKET e=expr RBRACKET {  SubscriptVar (v, e, to_pos($startpos)) }