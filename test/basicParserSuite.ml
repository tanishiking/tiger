open OUnit2
open Util
open Syntax.Ast
open Syntax.Symbol

let basic_suite =
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
         basic_parse_test "basic_call"
           (CallExp
              {
                func = fake_sym "func";
                args =
                  [
                    VarExp (SimpleVar (fake_sym "x", fake_pos));
                    VarExp (SimpleVar (fake_sym "y", fake_pos));
                  ];
                pos = fake_pos;
              })
           "func(x, y)";
         basic_parse_test "call_empty"
           (CallExp { func = fake_sym "func"; args = []; pos = fake_pos })
           "func()";
       ]
