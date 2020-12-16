%{
  open Ast
%}

%token <int>           INT
%token <string>        STR
%token <Symbol.symbol> ID
%token                 EOF
%token NIL DOT COMMA SEMICOLON COLON
%token ASSIGN
%token LBRACKET RBRACKET
%token LPAREN RPAREN
%token LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE LAND LOR
%token WHILE DO
%token IF THEN ELSE
%token FOR TO
%token AT
%token BREAK OF
%token LET IN END VAR FUNCTION AND TYPE ARRAY

%nonassoc DO THEN OF
%nonassoc ELSE
%nonassoc ASSIGN

%left LOR
%left LAND
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
  | NIL { NilExp }
  | IF test=expr THEN t=expr
    { IfExp { test=test; then'=t; else'=None; pos=to_pos($startpos)} }
  | IF test=expr THEN t=expr ELSE e=expr
    { IfExp { test=test; then'=t; else'=Some e; pos=to_pos($startpos)} }
  | WHILE test=expr DO body=expr
    { WhileExp {test=test; body=body; pos=to_pos($startpos)} }
  | FOR id=ID ASSIGN lo=expr TO hi=expr DO body=expr
    { ForExp { var=id; lo=lo; hi=hi; body=body; pos=to_pos($startpos) } }
  | BREAK
    { BreakExp (to_pos($startpos)) }
  | LPAREN es=separated_list(SEMICOLON, exppos) RPAREN
    { SeqExp es }
  | i = INT
    { IntExp i }
  | MINUS e = expr %prec UMINUS
    { OpExp { left=IntExp 0; oper=MinusOp; right=e; pos=to_pos($startpos)} }
  | l=expr o=binop r=expr
    { OpExp { left=l; oper=o; right=r; pos=to_pos($startpos) } }
  | s = STR
    { StringExp (s, to_pos($startpos)) }
  | v = var
    { VarExp v }
  | lvalue=var ASSIGN e=expr
    { AssignExp {var=lvalue; exp=e; pos=to_pos($startpos)} }
  | name=ID LPAREN args=separated_list(COMMA, expr) RPAREN
    { CallExp { func=name; args=args; pos=to_pos($startpos) } }
  | name=ID LBRACE fields=separated_list(COMMA, field) RBRACE
    { RecordExp { fields=fields; typ=name; pos=to_pos($startpos) } }
  | /* Added @ between ID and LBRACKET to avoid conflict (is there a way to avoid it ?) */
    typ=ID AT LBRACKET size=expr RBRACKET OF init=expr
    { ArrayExp { typ=typ; size=size; init=init; pos=to_pos($startpos) } }
  | LET decs=list(dec) IN expseq=separated_list(SEMICOLON, exppos) END
    { LetExp { decs=decs; body=(SeqExp expseq); pos=to_pos($startpos) } }

dec : 
  | d=vardec { d }
  | fs=separated_nonempty_list(AND, fundec) /* Add "and" between fundecs to avoid shift/reduce conflict */
    { FunctionDec fs }
  | ts=separated_nonempty_list(AND, tydec) /* Add "and" between fundecs to avoid shift/reduce conflict */
    { TypeDec ts }

vardec:
  | VAR v=ID t=type_constraint ASSIGN e=expr
    { VarDec { name=v; typ=t; init=e; pos=to_pos($startpos) } }

type_constraint:
  | c=option(COLON t=ID { (t, to_pos($startpos)) })
    { c }

fundec:
  | FUNCTION name=ID LPAREN params=separated_list(COMMA, tyfield) RPAREN result=type_constraint EQ body=expr
    { { funname=name; params=params; result=result; body=body; funpos=to_pos($startpos)} }

tyfield :
  | name=ID COLON typ=ID
    { { name=name; typ=typ; pos=to_pos($startpos) } }

tydec:
 | TYPE name=ID EQ ty=ty
   { { tyname=name; ty=ty; typos=to_pos($startpos) } }

ty:
 | ty=ID
   { NameTy (ty, to_pos($startpos))}
 | LBRACE fields=separated_list(COMMA, tyfield) RBRACE
   { RecordTy fields }
 | ARRAY OF typ=ID
   { ArrayTy (typ, to_pos($startpos)) }

var:
  | v=ID  { SimpleVar (v, to_pos($startpos)) }
  | v=var DOT f=ID { FieldVar (v, f, to_pos($startpos)) }
  | v=var LBRACKET e=expr RBRACKET { SubscriptVar (v, e, to_pos($startpos)) }

field :
  | k=ID EQ v=expr { (k, v, to_pos($startpos)) }

exppos :
  | e=expr { (e, to_pos($startpos)) }

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
  | LAND { AndOp }
  | LOR { OrOp }