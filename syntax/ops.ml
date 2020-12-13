open Ast

let string_of_pos (p : pos) =
  "{ bol = " ^ string_of_int p.bol ^ ", lnum = " ^ string_of_int p.lnum ^ "}"

let pretty exp =
  match exp with
  | IntExp i -> string_of_int i
  | StringExp (s, p) -> s ^ string_of_pos p
  | _ -> "not implemented"
