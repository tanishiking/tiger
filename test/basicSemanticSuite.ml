open OUnit2
open Util
open Core.Tpd
open Core.Symbol
module Syminfo = Core.Syminfo
module T = Core.Types

let basic_suite =
  "Basic"
  >::: [
         basic_semantic_test "nil" TypedNilExp "nil";
         basic_semantic_test "digit" (TypedIntExp 1) "1";
         basic_semantic_test "basic_string"
           (TypedStringExp ("foo", fake_pos))
           "\"foo\"";
         basic_semantic_test "basic_let"
           (TypedLetExp
              {
                decs =
                  [
                    TypedVarDec
                      {
                        name =
                          {
                            sym = symbol "x";
                            kind = Syminfo.VAR;
                            tpe = T.INT;
                            role = Syminfo.DEF;
                            level = 0;
                          };
                        typ = None;
                        init = TypedIntExp 1;
                        pos = fake_pos;
                      };
                  ];
                body =
                  TypedSeqExp
                    [
                      ( TypedVarExp
                          (TypedSimpleVar
                             ( {
                                 sym = symbol "x";
                                 kind = Syminfo.VAR;
                                 tpe = T.INT;
                                 role = Syminfo.REF;
                                 level = 0;
                               },
                               fake_pos )),
                        fake_pos );
                    ];
                pos = fake_pos;
              })
           "let var x := 1 in x end";
       ]
