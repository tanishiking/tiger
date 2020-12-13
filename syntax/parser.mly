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
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE AND OR

%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT GT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start <Ast.exp> prog

%%

prog:
    | e = expr EOF
      { e }

expr:
  | i = INT
    { IntExp i }
  | MINUS e = expr %prec UMINUS
    { OpExp { left=IntExp 0; oper=MinusOp; right=e; pos=to_pos($startpos)} }
  | l=expr o=binop r=expr
    { OpExp { left=l; oper=o; right=r; pos=to_pos($startpos) } }
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

%inline binop:
  | PLUS { PlusOp }
  | MINUS { MinusOp }
  | TIMES { TimesOp }
  | DIVIDE { DivideOp }
  | EQ { EqOp }
  | NEQ { NeqOp }
  | LT { LtOp }
  | LE { LeOp }
  | GT { GtOp }
  | GE { GeOp }
  | AND { AndOp }
  | OR { OrOp }