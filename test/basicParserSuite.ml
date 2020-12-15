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
         basic_parse_test "basic_record_creation"
           (RecordExp
              {
                fields =
                  [
                    (fake_sym "line", IntExp 1, fake_pos);
                    (fake_sym "col", IntExp 2, fake_pos);
                  ];
                typ = fake_sym "pos";
                pos = fake_pos;
              })
           "pos { line = 1, col = 2 }";
         basic_parse_test "record_creation_empty"
           (RecordExp { fields = []; typ = fake_sym "pos"; pos = fake_pos })
           "pos { }";
         basic_parse_test "empty_sequencing" (SeqExp []) "()";
         basic_parse_test "sequencing_int_int"
           (SeqExp [ (IntExp 1, fake_pos); (IntExp 2, fake_pos) ])
           "(1; 2)";
         basic_parse_test "sequencing_assign_int"
           (SeqExp
              [
                ( AssignExp
                    {
                      var = SimpleVar (fake_sym "x", fake_pos);
                      exp = IntExp 1;
                      pos = fake_pos;
                    },
                  fake_pos );
                ( OpExp
                    {
                      left = VarExp (SimpleVar (fake_sym "x", fake_pos));
                      oper = PlusOp;
                      right = IntExp 1;
                      pos = fake_pos;
                    },
                  fake_pos );
              ])
           "(x := 1; x + 1)";
         basic_parse_test "if_then"
           (IfExp
              {
                test =
                  OpExp
                    {
                      left = VarExp (SimpleVar (fake_sym "x", fake_pos));
                      oper = EqOp;
                      right = IntExp 1;
                      pos = fake_pos;
                    };
                then' = VarExp (SimpleVar (fake_sym "x", fake_pos));
                else' = None;
                pos = fake_pos;
              })
           "if x = 1 then x";
         basic_parse_test "if_then_else"
           (IfExp
              {
                test =
                  OpExp
                    {
                      left = VarExp (SimpleVar (fake_sym "x", fake_pos));
                      oper = EqOp;
                      right = IntExp 1;
                      pos = fake_pos;
                    };
                then' = VarExp (SimpleVar (fake_sym "x", fake_pos));
                else' = Some (IntExp 1);
                pos = fake_pos;
              })
           "if x = 1 then x else 1";
         basic_parse_test "while_do"
           (WhileExp
              {
                test =
                  OpExp
                    {
                      left = VarExp (SimpleVar (fake_sym "x", fake_pos));
                      oper = LtOp;
                      right = IntExp 10;
                      pos = fake_pos;
                    };
                body =
                  AssignExp
                    {
                      var = SimpleVar (fake_sym "x", fake_pos);
                      exp =
                        OpExp
                          {
                            left = VarExp (SimpleVar (fake_sym "x", fake_pos));
                            oper = PlusOp;
                            right = IntExp 1;
                            pos = fake_pos;
                          };
                      pos = fake_pos;
                    };
                pos = fake_pos;
              })
           "while x < 10 do x := x + 1";
         basic_parse_test "for_loop"
           (ForExp
              {
                var = fake_sym "x";
                lo = IntExp 0;
                hi = IntExp 10;
                body =
                  AssignExp
                    {
                      var = SimpleVar (fake_sym "x", fake_pos);
                      exp =
                        OpExp
                          {
                            left = VarExp (SimpleVar (fake_sym "x", fake_pos));
                            oper = PlusOp;
                            right = IntExp 1;
                            pos = fake_pos;
                          };
                      pos = fake_pos;
                    };
                pos = fake_pos;
              })
           "for x := 0 to 10 do x := x + 1";
         basic_parse_test "break" (BreakExp fake_pos) "break";
         basic_parse_test "array"
           (ArrayExp
              {
                typ = fake_sym "str";
                size = IntExp 100;
                init = StringExp ("", fake_pos);
                pos = fake_pos;
              })
           "str @ [100] of \"\"";
       ]
