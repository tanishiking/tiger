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
         basic_parse_test "function"
           (LetExp
              {
                decs =
                  [
                    FunctionDec
                      [
                        {
                          funname = fake_sym "inc";
                          params =
                            [
                              {
                                name = fake_sym "x";
                                typ = fake_sym "int";
                                pos = fake_pos;
                              };
                            ];
                          result = Some (fake_sym "int", fake_pos);
                          body =
                            OpExp
                              {
                                left =
                                  VarExp (SimpleVar (fake_sym "x", fake_pos));
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
                          {
                            func = fake_sym "inc";
                            args = [ IntExp 1 ];
                            pos = fake_pos;
                          },
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
                          funname = fake_sym "ping";
                          params = [];
                          result = None;
                          body =
                            CallExp
                              {
                                func = fake_sym "pong";
                                args = [];
                                pos = fake_pos;
                              };
                          funpos = fake_pos;
                        };
                        {
                          funname = fake_sym "pong";
                          params = [];
                          result = None;
                          body =
                            CallExp
                              {
                                func = fake_sym "ping";
                                args = [];
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
                          { func = fake_sym "ping"; args = []; pos = fake_pos },
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
                          tyname = fake_sym "keyid";
                          ty = NameTy (fake_sym "int", fake_pos);
                          typos = fake_pos;
                        };
                        {
                          tyname = fake_sym "tree";
                          ty =
                            RecordTy
                              [
                                {
                                  name = fake_sym "key";
                                  typ = fake_sym "keyid";
                                  pos = fake_pos;
                                };
                                {
                                  name = fake_sym "children";
                                  typ = fake_sym "treelist";
                                  pos = fake_pos;
                                };
                              ];
                          typos = fake_pos;
                        };
                        {
                          tyname = fake_sym "treelist";
                          ty = ArrayTy (fake_sym "tree", fake_pos);
                          typos = fake_pos;
                        };
                      ];
                  ];
                body =
                  SeqExp
                    [
                      ( CallExp
                          {
                            func = fake_sym "print";
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
                          tyname = fake_sym "keyid";
                          ty = NameTy (fake_sym "int", fake_pos);
                          typos = fake_pos;
                        };
                      ];
                    FunctionDec
                      [
                        {
                          funname = fake_sym "inc";
                          params =
                            [
                              {
                                name = fake_sym "x";
                                typ = fake_sym "int";
                                pos = fake_pos;
                              };
                            ];
                          result = Some (fake_sym "int", fake_pos);
                          body =
                            OpExp
                              {
                                left =
                                  VarExp (SimpleVar (fake_sym "x", fake_pos));
                                oper = PlusOp;
                                right = IntExp 1;
                                pos = fake_pos;
                              };
                          funpos = fake_pos;
                        };
                      ];
                    VarDec
                      {
                        name = fake_sym "v";
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
                            func = fake_sym "print";
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
