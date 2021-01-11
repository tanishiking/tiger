open OUnit2
open Syntax
open Core.Ast

let parse p : exp =
  let lexbuf = Lexing.from_string p in
  Parser.prog Lexer.token lexbuf

let fake_pos : pos = { lnum = 0; bol = 0 }

(**
  @see https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/data/ounit_improved_output.html
*)
let basic_parse_test name expected input =
  name >:: fun _ ->
  assert_equal expected (parse input) ~printer:show_exp ~cmp:equal_exp
