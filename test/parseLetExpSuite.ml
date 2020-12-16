open OUnit2
open Util
open Syntax.Ast

let parse_letexp_suite =
  "ParseLetExp"
  >::: [
         basic_parse_test "vardec_no_typeconstraint"
           (LetExp
              {
                decs =
                  [
                    VarDec
                      {
                        name = fake_sym "v";
                        typ = None;
                        init = IntExp 6;
                        pos = fake_pos;
                      };
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp
                          {
                            func = fake_sym "print";
                            args =
                              [ VarExp (SimpleVar (fake_sym "v", fake_pos)) ];
                            pos = fake_pos;
                          },
                        fake_pos );
                    ];
                pos = fake_pos;
              })
           "let var v := 6 in print(v) end";
         basic_parse_test "vardec_with_typeconstraint"
           (LetExp
              {
                decs =
                  [
                    VarDec
                      {
                        name = fake_sym "v";
                        typ = Some (fake_sym "int", fake_pos);
                        init = IntExp 6;
                        pos = fake_pos;
                      };
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp
                          {
                            func = fake_sym "print";
                            args =
                              [ VarExp (SimpleVar (fake_sym "v", fake_pos)) ];
                            pos = fake_pos;
                          },
                        fake_pos );
                    ];
                pos = fake_pos;
              })
           "let var v: int := 6 in print(v) end";
         basic_parse_test "vardecs"
           (LetExp
              {
                decs =
                  [
                    VarDec
                      {
                        name = fake_sym "x";
                        typ = Some (fake_sym "int", fake_pos);
                        init = IntExp 6;
                        pos = fake_pos;
                      };
                    VarDec
                      {
                        name = fake_sym "y";
                        typ = Some (fake_sym "string", fake_pos);
                        init = StringExp ("test", fake_pos);
                        pos = fake_pos;
                      };
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp
                          {
                            func = fake_sym "print";
                            args =
                              [ VarExp (SimpleVar (fake_sym "x", fake_pos)) ];
                            pos = fake_pos;
                          },
                        fake_pos );
                      ( CallExp
                          {
                            func = fake_sym "print";
                            args =
                              [ VarExp (SimpleVar (fake_sym "y", fake_pos)) ];
                            pos = fake_pos;
                          },
                        fake_pos );
                    ];
                pos = fake_pos;
              })
           "let var x: int := 6 var y: string := \"test\" in print(x); \
            print(y) end";
         basic_parse_test "nested_let"
           (LetExp
              {
                decs =
                  [
                    VarDec
                      {
                        name = fake_sym "x";
                        typ = Some (fake_sym "int", fake_pos);
                        init = IntExp 6;
                        pos = fake_pos;
                      };
                  ];
                body =
                  SeqExp
                    [
                      ( LetExp
                          {
                            decs =
                              [
                                VarDec
                                  {
                                    name = fake_sym "y";
                                    typ = Some (fake_sym "int", fake_pos);
                                    init = IntExp 6;
                                    pos = fake_pos;
                                  };
                              ];
                            body =
                              SeqExp
                                [
                                  ( CallExp
                                      {
                                        func = fake_sym "print";
                                        args =
                                          [
                                            VarExp
                                              (SimpleVar (fake_sym "x", fake_pos));
                                          ];
                                        pos = fake_pos;
                                      },
                                    fake_pos );
                                ];
                            pos = fake_pos;
                          },
                        fake_pos );
                    ];
                pos = fake_pos;
              })
           "let var x: int := 6 in let var y: int := 6 in print(x) end end";
       ]
