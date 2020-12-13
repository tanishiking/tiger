open OUnit2
open Syntax
open Cmp
open Syntax.Ast
open Syntax.Ops
open Syntax.Symbol

let parse p : exp =
  let lexbuf = Lexing.from_string p in
  Parser.prog Lexer.token lexbuf

let fake_pos : pos = { lnum = 0; bol = 0 }

let fake_sym name = mk_symbol name 0

(**
  @see https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/data/ounit_improved_output.html
*)
let basic_parse_test name expected input =
  name >:: fun _ ->
  assert_equal expected (parse input) ~printer:pretty_exp ~cmp:cmp_exp

let suite =
  "Basic"
  >::: [
         basic_parse_test "nil" NilExp "nil";
         basic_parse_test "spaces_digit" (IntExp 1) " 1 ";
         basic_parse_test "single_digit" (IntExp 1) "1";
         basic_parse_test "multiple_digit" (IntExp 1234) "1234";
         basic_parse_test "basic_string" (StringExp ("foo", fake_pos)) "\"foo\"";
         basic_parse_test "simple_var"
           (VarExp (SimpleVar (fake_sym "id", fake_pos)))
           "id";
         basic_parse_test "field_var"
           (VarExp
              (FieldVar
                 (SimpleVar (fake_sym "obj", fake_pos), fake_sym "x", fake_pos)))
           "obj.x";
         basic_parse_test "field_chain"
           (VarExp
              (FieldVar
                 ( FieldVar
                     ( SimpleVar (fake_sym "obj", fake_pos),
                       fake_sym "x",
                       fake_pos ),
                   fake_sym "y",
                   fake_pos )))
           "obj.x.y";
         basic_parse_test "simple_subscription"
           (VarExp
              (SubscriptVar
                 (SimpleVar (fake_sym "list", fake_pos), IntExp 0, fake_pos)))
           "list[0]";
         basic_parse_test "simple_assign"
           (AssignExp
              {
                var = SimpleVar (mk_symbol "obj" 0, fake_pos);
                exp = IntExp 1;
                pos = fake_pos;
              })
           "obj := 1";
       ]

let () = run_test_tt_main suite
