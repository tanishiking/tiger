module Symtab = Symtab
module T = Core.Types
module Symbol = Core.Symbol

type enventry =
  | VarEntry of { ty : T.tpe; const : bool }
  | FunEntry of { formals : T.tpe list; result : T.tpe }

let standard_types = [ ("int", T.INT); ("string", T.STRING) ]

let standard_functions =
  [ ("printint", [ T.INT ], T.UNIT); ("exit", [ T.INT ], T.UNIT) ]

type venv = enventry Symtab.table

type tyenv = T.tpe Symtab.table

let base_venv : venv =
  let makeFunEntry name params result =
    (name, FunEntry { formals = params; result })
  in
  List.fold_left
    (fun env (name, formals, result) ->
      let name, entry = makeFunEntry name formals result in
      Symtab.enter (Symbol.symbol name) entry env)
    Symtab.empty standard_functions

let base_tenv : tyenv =
  List.fold_left
    (fun env (name, t) -> Symtab.enter (Symbol.symbol name) t env)
    Symtab.empty standard_types
