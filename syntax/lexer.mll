{
open Parser
}

let digit = ['0'-'9']
let num = '-'? digit digit*
let ws = ['\t' ' ' '\n']

rule token = parse
  | ws+       { token lexbuf }
  | num as n  { INT (int_of_string n) }
  | '"'       { string (Buffer.create 0) lexbuf }
  | eof       { EOF }

and string buf = parse
  | '"'       { STR (Buffer.contents buf) }
  | _ as s { Buffer.add_char buf s; string buf lexbuf }