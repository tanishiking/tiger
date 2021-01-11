open OUnit2
open Util
open Core.Ast

let parse_binop_suite =
  "ParseBinOp"
  >::: [
         basic_parse_test "simple_plus"
           (OpExp
              {
                left = IntExp 1;
                oper = PlusOp;
                right = IntExp 1;
                pos = fake_pos;
              })
           "1 + 1";
         basic_parse_test "times_>_plus"
           (OpExp
              {
                left = IntExp 1;
                oper = MinusOp;
                right =
                  OpExp
                    {
                      left = IntExp 1;
                      oper = TimesOp;
                      right = IntExp 1;
                      pos = fake_pos;
                    };
                pos = fake_pos;
              })
           "1 - 1 * 1";
         basic_parse_test "divide_>_minus"
           (OpExp
              {
                left = IntExp 1;
                oper = MinusOp;
                right =
                  OpExp
                    {
                      left = IntExp 1;
                      oper = DivideOp;
                      right = IntExp 1;
                      pos = fake_pos;
                    };
                pos = fake_pos;
              })
           "1 - 1 / 1";
         basic_parse_test "divide_leftassoc_times"
           (OpExp
              {
                left =
                  OpExp
                    {
                      left = IntExp 1;
                      oper = TimesOp;
                      right = IntExp 1;
                      pos = fake_pos;
                    };
                oper = DivideOp;
                right = IntExp 1;
                pos = fake_pos;
              })
           "1 * 1 / 1";
         basic_parse_test "ltlegtge_>_andor"
           (OpExp
              {
                left = IntExp 1;
                oper = AndOp;
                right =
                  OpExp
                    {
                      left = IntExp 1;
                      oper = GeOp;
                      right = IntExp 1;
                      pos = fake_pos;
                    };
                pos = fake_pos;
              })
           "1 & 1 >= 1";
         basic_parse_test "uminus"
           (OpExp
              {
                left =
                  OpExp
                    {
                      left = IntExp 0;
                      oper = MinusOp;
                      right = IntExp 1;
                      pos = fake_pos;
                    };
                oper = TimesOp;
                right = IntExp 5;
                pos = fake_pos;
              })
           "-1 * 5";
       ]
