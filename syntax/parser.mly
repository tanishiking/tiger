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
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE AND OR
%token WHILE DO
%token IF THEN ELSE
%token FOR TO
%token AT
%token BREAK OF
%token LET IN END VAR

%nonassoc DO THEN OF
%nonassoc ELSE
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
  | LPAREN es=expseq RPAREN
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
  | name=ID LPAREN args=argseq RPAREN
    { CallExp { func=name; args=args; pos=to_pos($startpos) } }
  | name=ID LBRACE fields=fieldseq RBRACE
    { RecordExp { fields=fields; typ=name; pos=to_pos($startpos) } }
  | /* Added @ between ID and LBRACKET to avoid conflict (is there a way to avoid it ?) */
    typ=ID AT LBRACKET size=expr RBRACKET OF init=expr
    { ArrayExp { typ=typ; size=size; init=init; pos=to_pos($startpos) } }
  | LET decs=decs IN expseq=expseq END
    { LetExp { decs=decs; body=(SeqExp expseq); pos=to_pos($startpos) } }

decs :
    /* empty */ {[]}
  | d=dec rest=decs {d :: rest}
  ;

dec : 
  | d=vardec { d }
  ;

vardec:
 | VAR v=ID t=type_constraint ASSIGN e=expr
   { VarDec { name=v; typ=t; init=e; pos=to_pos($startpos) } }

type_constraint:
 | c=option(COLON t=ID { (t, to_pos($startpos)) })
   { c }

var:
  | v=ID  { SimpleVar (v, to_pos($startpos)) }
  | v=var DOT f=ID { FieldVar (v, f, to_pos($startpos)) }
  | v=var LBRACKET e=expr RBRACKET { SubscriptVar (v, e, to_pos($startpos)) }

argseq:
    /* empty */ {[]}
  | expr argseq_ {$1 :: $2}
  ;

argseq_:
    /* empty */ {[]}
  | COMMA expr argseq_ {$2 :: $3}

fieldseq :
    /* empty */ {[]}
  | k=ID EQ v=expr rest=fieldseq_ {(k, v, to_pos($startpos)) :: rest}
  ;

fieldseq_ :
    /* empty */ {[]}
  | COMMA k=ID EQ v=expr rest=fieldseq_ {(k, v, to_pos($startpos)) :: rest}
  ;

expseq :
    /* empty */ {[]}
  | expr expseq_ { ($1, to_pos($startpos)) :: $2}
  ;

expseq_ :
    /* empty */ {[]}
  | SEMICOLON expr expseq_ { ($2, to_pos($startpos)) :: $3}
  ;

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