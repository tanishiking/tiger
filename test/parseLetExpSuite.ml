open OUnit2
open Util
open Core.Ast

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
                        name = "v";
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
                            func = "print";
                            args = [ VarExp (SimpleVar ("v", fake_pos)) ];
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
                        name = "v";
                        typ = Some ("int", fake_pos);
                        init = IntExp 6;
                        pos = fake_pos;
                      };
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp
                          {
                            func = "print";
                            args = [ VarExp (SimpleVar ("v", fake_pos)) ];
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
                        name = "x";
                        typ = Some ("int", fake_pos);
                        init = IntExp 6;
                        pos = fake_pos;
                      };
                    VarDec
                      {
                        name = "y";
                        typ = Some ("string", fake_pos);
                        init = StringExp ("test", fake_pos);
                        pos = fake_pos;
                      };
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp
                          {
                            func = "print";
                            args = [ VarExp (SimpleVar ("x", fake_pos)) ];
                            pos = fake_pos;
                          },
                        fake_pos );
                      ( CallExp
                          {
                            func = "print";
                            args = [ VarExp (SimpleVar ("y", fake_pos)) ];
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
                        name = "x";
                        typ = Some ("int", fake_pos);
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
                                    name = "y";
                                    typ = Some ("int", fake_pos);
                                    init = IntExp 6;
                                    pos = fake_pos;
                                  };
                              ];
                            body =
                              SeqExp
                                [
                                  ( CallExp
                                      {
                                        func = "print";
                                        args =
                                          [ VarExp (SimpleVar ("x", fake_pos)) ];
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
         basic_parse_test "function"
           (LetExp
              {
                decs =
                  [
                    FunctionDec
                      [
                        {
                          funname = "inc";
                          params =
                            [ { name = "x"; typ = "int"; pos = fake_pos } ];
                          result = Some ("int", fake_pos);
                          body =
                            OpExp
                              {
                                left = VarExp (SimpleVar ("x", fake_pos));
                                oper = PlusOp;
                                right = IntExp 1;
                                pos = fake_pos;
                              };
                          funpos = fake_pos;
                        };
                      ];
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp
                          { func = "inc"; args = [ IntExp 1 ]; pos = fake_pos },
                        fake_pos );
                    ];
                pos = fake_pos;
              })
           "let function inc (x: int): int = x + 1 in inc(1) end";
         basic_parse_test "mutual_function"
           (LetExp
              {
                decs =
                  [
                    FunctionDec
                      [
                        {
                          funname = "ping";
                          params = [];
                          result = None;
                          body =
                            CallExp { func = "pong"; args = []; pos = fake_pos };
                          funpos = fake_pos;
                        };
                        {
                          funname = "pong";
                          params = [];
                          result = None;
                          body =
                            CallExp { func = "ping"; args = []; pos = fake_pos };
                          funpos = fake_pos;
                        };
                      ];
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp { func = "ping"; args = []; pos = fake_pos },
                        fake_pos );
                    ];
                pos = fake_pos;
              })
           "let function ping() = pong() and function pong() = ping() in \
            ping() end";
         basic_parse_test "basic_typedec"
           (LetExp
              {
                decs =
                  [
                    TypeDec
                      [
                        {
                          tyname = "keyid";
                          ty = NameTy ("int", fake_pos);
                          typos = fake_pos;
                        };
                        {
                          tyname = "tree";
                          ty =
                            RecordTy
                              [
                                { name = "key"; typ = "keyid"; pos = fake_pos };
                                {
                                  name = "children";
                                  typ = "treelist";
                                  pos = fake_pos;
                                };
                              ];
                          typos = fake_pos;
                        };
                        {
                          tyname = "treelist";
                          ty = ArrayTy ("tree", fake_pos);
                          typos = fake_pos;
                        };
                      ];
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp
                          {
                            func = "print";
                            args = [ IntExp 1 ];
                            pos = fake_pos;
                          },
                        fake_pos );
                    ];
                pos = fake_pos;
              })
           "let type keyid = int and type tree = { key: keyid, children: \
            treelist } and type treelist = array of tree in print(1) end";
         basic_parse_test "mixed_decs"
           (LetExp
              {
                decs =
                  [
                    TypeDec
                      [
                        {
                          tyname = "keyid";
                          ty = NameTy ("int", fake_pos);
                          typos = fake_pos;
                        };
                      ];
                    FunctionDec
                      [
                        {
                          funname = "inc";
                          params =
                            [ { name = "x"; typ = "int"; pos = fake_pos } ];
                          result = Some ("int", fake_pos);
                          body =
                            OpExp
                              {
                                left = VarExp (SimpleVar ("x", fake_pos));
                                oper = PlusOp;
                                right = IntExp 1;
                                pos = fake_pos;
                              };
                          funpos = fake_pos;
                        };
                      ];
                    VarDec
                      {
                        name = "v";
                        typ = None;
                        init = IntExp 1;
                        pos = fake_pos;
                      };
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp
                          {
                            func = "print";
                            args = [ IntExp 1 ];
                            pos = fake_pos;
                          },
                        fake_pos );
                    ];
                pos = fake_pos;
              })
           "let type keyid = int function inc (x: int): int = x + 1 var v := 1 \
            in print(1) end";
       ]
