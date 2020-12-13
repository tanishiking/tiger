open Ast
open Symbol

let string_of_pos (p : pos) =
  "{ bol = " ^ string_of_int p.bol ^ ", lnum = " ^ string_of_int p.lnum ^ "}"

let rec pretty_var var =
  match var with
  | SimpleVar (sym, p) -> string_of_symbol sym ^ string_of_pos p
  | FieldVar (var, sym, p) ->
      pretty_var var ^ "." ^ string_of_symbol sym ^ string_of_pos p
  | SubscriptVar (var, exp, p) ->
      pretty_var var ^ "[" ^ pretty_exp exp ^ "]" ^ string_of_pos p

and pretty_exp exp =
  match exp with
  | NilExp -> "nil"
  | IntExp i -> string_of_int i
  | StringExp (s, p) -> s ^ string_of_pos p
  | VarExp var -> pretty_var var
  | AssignExp { var = v; exp = e; pos = p } ->
      pretty_var v ^ ":=" ^ pretty_exp e ^ string_of_pos p
  | OpExp { left = l; oper = op; right = r; pos = p } ->
      pretty_exp l ^ pretty_op op ^ pretty_exp r ^ string_of_pos p
  | _ -> "not implemented"

and pretty_op op =
  match op with
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | DivideOp -> "/"
  | EqOp -> "="
  | NeqOp -> "<>"
  | LtOp -> "<"
  | GtOp -> ">"
  | LeOp -> "<="
  | GeOp -> ">="
  | AndOp -> "&"
  | OrOp -> "|"
