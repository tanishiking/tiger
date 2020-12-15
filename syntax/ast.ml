open Symbol

type symbol = Symbol.symbol

type pos = {
  lnum : int; [@equal fun _ _ -> true]
  bol : int; [@equal fun _ _ -> true]
}
[@@deriving show, eq]

let to_pos (p : Lexing.position) : pos = { lnum = p.pos_lnum; bol = p.pos_bol }

type var =
  | SimpleVar of symbol * pos
  | FieldVar of var * symbol * pos
  | SubscriptVar of var * exp * pos
[@@deriving show, eq]

and exp =
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | VarExp of var
  | AssignExp of { var : var; exp : exp; pos : pos }
  | OpExp of { left : exp; oper : oper; right : exp; pos : pos }
  | CallExp of { func : symbol; args : exp list; pos : pos }
  | RecordExp of { fields : (symbol * exp * pos) list; typ : symbol; pos : pos }
  | SeqExp of (exp * pos) list
  | IfExp of { test : exp; then' : exp; else' : exp option; pos : pos }
  | WhileExp of { test : exp; body : exp; pos : pos }
  | ForExp of { var : symbol; lo : exp; hi : exp; body : exp; pos : pos }
  | BreakExp of pos
  | ArrayExp of { typ : symbol; size : exp; init : exp; pos : pos }
  | LetExp of { decs : dec list; body : exp; pos : pos }
[@@deriving show, eq]

and dec =
  | FunctionDec of fundec list
  | VarDec of {
      name : symbol;
      escape : bool ref;
      typ : (symbol * pos) option;
      init : exp;
      pos : pos;
    }
  | TypeDec of typedec list
[@@deriving show, eq]

and ty =
  | NameTy of symbol * pos
  | RecordTy of field list
  | ArrayTy of symbol * pos
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

and field = { name : symbol; escape : bool ref; typ : symbol; pos : pos }
[@@deriving show, eq]

and fundec = {
  funname : symbol;
  params : field list;
  result : (symbol * pos) option;
  body : exp;
  funpos : pos;
}
[@@deriving show, eq]

and typedec = { tyname : symbol; ty : ty; typos : pos } [@@deriving show, eq]
