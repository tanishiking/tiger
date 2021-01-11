type kind = VAR | FUNC | TYPE [@@deriving show, eq]

type role = DEF | REF [@@deriving show, eq]

type info = {
  sym : Symbol.symbol;
  kind : kind;
  tpe : Types.tpe;
  role : role;
  level : int;
}
[@@deriving show, eq]
