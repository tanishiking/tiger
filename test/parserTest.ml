open OUnit2
open Syntax
open Syntax.Ast
open Syntax.Ops

let parse p : exp =
  let lexbuf = Lexing.from_string p in
  Parser.prog Lexer.token lexbuf

(**
  @see https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/data/ounit_improved_output.html
*)
let basic_parse_test name expected input =
  name >:: fun _ -> assert_equal expected (parse input) ~printer:pretty

let test_parse_string_expr _ =
  assert_equal (StringExp ("foo", { bol = 2; lnum = 0 })) (parse "\"foo\"")

let suite =
  "Basic"
  >::: [
         basic_parse_test "single_digit" (IntExp 1) "1";
         basic_parse_test "multiple_digit" (IntExp 1234) "1234";
         basic_parse_test "basic_string"
           (StringExp ("foo", { bol = 0; lnum = 1 }))
           "\"foo\"";
       ]

let () = run_test_tt_main suite
