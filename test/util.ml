open OUnit2
open Syntax
open Core.Ast
open Core.Sourcepos
open Core.Tpd
open Semantic
module Error = Core.Error

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

let basic_semantic_test name expected input =
  name >:: fun _ ->
  assert_equal expected
    (type_prog (parse input))
    ~printer:show_typed_exp ~cmp:equal_typed_exp

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let semantic_error_test name input expected =
  name >:: fun _ ->
  try
    ignore (type_prog (parse input));
    assert_failure "Error not raised"
  with
  | Error.Error (_, msg) ->
      if not (contains msg expected) then
        assert_failure
          (Printf.sprintf "Error message \"%s\" doesn't contains \"%s\"" msg
             expected)
  | _ -> assert_failure "Unexpected exception raised."
