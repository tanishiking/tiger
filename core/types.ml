type unique = unit ref [@@deriving show, eq]

type tpe =
  | UNIT
  | NIL
  | INT
  | STRING
  | RECORD of (Symbol.symbol * tpe) list * unique
  | ARRAY of tpe * unique
  | FUNCTION of tpe list * tpe
  (* ref to tpe, Name(sym, Some(t)) equals t, Name(sym, None) is placeholder *)
  | NAME of Symbol.symbol * tpe option ref
[@@deriving show, eq]

let rec actual_ty = function
  (* constatns is the value of ref *)
  | NAME (_, { contents = Some t }) -> actual_ty t
  | t -> t

let rec coerceable a b =
  match (a, b) with
  | UNIT, UNIT -> true
  | NIL, NIL -> true
  (* | NIL, RECORD _ -> true (* ?? *) *)
  | INT, INT -> true
  | STRING, STRING -> true
  | NAME (_, { contents = Some t }), b -> coerceable t b
  | a, NAME (_, { contents = Some t }) -> coerceable a t
  | RECORD (_, uniqa), RECORD (_, uniqb) -> uniqa = uniqb
  | ARRAY (_, uniqa), ARRAY (_, uniqb) -> uniqa = uniqb
  | FUNCTION _, FUNCTION _ -> false
  | _ -> false
