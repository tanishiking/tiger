open Name

type pos = {
  lnum : int; [@equal fun _ _ -> true]
  bol : int; [@equal fun _ _ -> true]
}
[@@deriving show, eq]

type var =
  | SimpleVar of name * pos
  | FieldVar of var * name * pos
  | SubscriptVar of var * exp * pos
[@@deriving show, eq]

and exp =
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | VarExp of var
  | AssignExp of { var : var; exp : exp; pos : pos }
  | OpExp of { left : exp; oper : oper; right : exp; pos : pos }
  | CallExp of { func : name; args : exp list; pos : pos }
  | RecordExp of { fields : (name * exp * pos) list; typ : name; pos : pos }
  | SeqExp of (exp * pos) list
  | IfExp of { test : exp; then' : exp; else' : exp option; pos : pos }
  | WhileExp of { test : exp; body : exp; pos : pos }
  | ForExp of { var : name; lo : exp; hi : exp; body : exp; pos : pos }
  | BreakExp of pos
  | ArrayExp of { typ : name; size : exp; init : exp; pos : pos }
  | LetExp of { decs : dec list; body : exp; pos : pos }
[@@deriving show, eq]

and dec =
  | FunctionDec of fundec list
  | VarDec of { name : name; typ : (name * pos) option; init : exp; pos : pos }
  | TypeDec of typedec list
[@@deriving show, eq]

and ty = NameTy of name * pos | RecordTy of field list | ArrayTy of name * pos
[@@deriving show, eq]

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  | AndOp
  | OrOp
[@@deriving show, eq]

and field = { name : name; typ : name; pos : pos } [@@deriving show, eq]

and fundec = {
  funname : name;
  params : field list;
  result : (name * pos) option;
  body : exp;
  funpos : pos;
}
[@@deriving show, eq]

and typedec = { tyname : name; ty : ty; typos : pos } [@@deriving show, eq]
