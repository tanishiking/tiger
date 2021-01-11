open Syminfo
open Sourcepos

(* typed AST *)

type typed_var =
  | TypedSimpleVar of info * pos
  | TypedFieldVar of typed_var * info * pos
  | TypedSubscriptVar of typed_var * typed_exp * pos
[@@deriving show, eq]

and typed_exp =
  | TypedNilExp
  | TypedIntExp of int
  | TypedStringExp of string * pos
  | TypedVarExp of typed_var
  | TypedAssignExp of { var : typed_var; exp : typed_exp; pos : pos }
  | TypedOpExp of {
      left : typed_exp;
      oper : oper;
      right : typed_exp;
      pos : pos;
    }
  | TypedCallExp of { func : info; args : typed_exp list; pos : pos }
  | TypedRecordExp of {
      fields : (info * typed_exp * pos) list;
      typ : info;
      pos : pos;
    }
  | TypedSeqExp of (typed_exp * pos) list
  | TypedIfExp of {
      test : typed_exp;
      then' : typed_exp;
      else' : typed_exp option;
      pos : pos;
    }
  | TypedWhileExp of { test : typed_exp; body : typed_exp; pos : pos }
  | TypedForExp of {
      var : info;
      lo : typed_exp;
      hi : typed_exp;
      body : typed_exp;
      pos : pos;
    }
  | TypedBreakExp of pos
  | TypedArrayExp of {
      typ : info;
      size : typed_exp;
      init : typed_exp;
      pos : pos;
    }
  | TypedLetExp of { decs : typed_dec list; body : typed_exp; pos : pos }
[@@deriving show, eq]

and typed_dec =
  | FunctionDec of funtyped_dec list
  | TypedVarDec of {
      name : info;
      typ : (info * pos) option;
      init : typed_exp;
      pos : pos;
    }
  | TypeDec of typetyped_dec list
[@@deriving show, eq]

and ty = NameTy of info * pos | RecordTy of field list | ArrayTy of info * pos
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

and field = { name : info; typ : info; pos : pos } [@@deriving show, eq]

and funtyped_dec = {
  funname : info;
  params : field list;
  result : (info * pos) option;
  body : typed_exp;
  funpos : pos;
}
[@@deriving show, eq]

and typetyped_dec = { tyname : info; ty : ty; typos : pos }
[@@deriving show, eq]
