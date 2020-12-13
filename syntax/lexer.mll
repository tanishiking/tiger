{
open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha+ (alpha | digit | '_')*
let num = digit digit*
let ws = ['\t' ' ' '\n']

rule token = parse
  | ws+       { token lexbuf }
  | "nil"     { NIL }
  | num as n  { INT (int_of_string n) }
  | '"'       { string (Buffer.create 0) lexbuf }
  | id as i   { ID (Symbol.symbol i) }
  | '.'       { DOT }
  | ":="      { ASSIGN }
  | "["       { LBRACKET }
  | "]"       { RBRACKET }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "/"       { DIVIDE }
  | "="       { EQ }
  | "<>"      { NEQ }
  | "<"       { LT }
  | ">"       { GT }
  | "<="      { LE }
  | ">="      { GE }
  | "&"       { AND }
  | "|"       { OR }
  | eof       { EOF }

and string buf = parse
  | '"'       { STR (Buffer.contents buf) }
  | _ as s { Buffer.add_char buf s; string buf lexbuf }