module A = Core.Ast
module S = Core.Symbol
module T = Core.Types
module E = Env
module Tpd = Core.Tpd
module Syminfo = Core.Syminfo

exception Foo of string

let coerce ty1 ty2 =
  if not (T.coerceable ty1 ty2) then
    (* TODO: return Error instead of raising error *)
    raise (Foo "type mismatch")

let lookup id env =
  match Symtab.lookup id env with
  | Some x -> x
  | None -> raise (Foo "undefined")

let vlookup id venv : E.enventry = lookup id venv

let tylookup id tyenv : T.tpe = lookup id tyenv

let type_var (_, venv, var) : T.tpe * Tpd.typed_var =
  match var with
  | A.SimpleVar (name, pos) -> (
      let sym = S.symbol name in
      match vlookup sym venv with
      | E.FunEntry _ -> raise (Foo "function cannot be here")
      | E.VarEntry { ty; _ } ->
          let info : Syminfo.info =
            { sym; kind = Syminfo.VAR; tpe = ty; role = Syminfo.REF; level = 0 }
          in
          (T.actual_ty ty, Tpd.TypedSimpleVar (info, pos)) )
  | _ -> raise (Foo "unimplemented")

let rec type_exp (((_ : E.tyenv), (_ : E.venv)) as env) exp :
    T.tpe * Tpd.typed_exp =
  match exp with
  | A.NilExp -> (T.NIL, Tpd.TypedNilExp)
  | A.IntExp i -> (T.INT, Tpd.TypedIntExp i)
  | A.StringExp (str, p) -> (T.STRING, Tpd.TypedStringExp (str, p))
  (*| A.VarExp v -> (type_var tenv, venv, v) *)
  | A.LetExp { decs; body; pos } ->
      let env', tpd_decs =
        List.fold_left
          (fun (env, typed_decs) dec ->
            let new_env, typed_dec = type_dec env dec in
            (new_env, typed_decs @ [ typed_dec ]))
          (env, []) decs
      in
      let t, typed_body = type_exp env' body in
      (t, TypedLetExp { decs = tpd_decs; body = typed_body; pos })
  | _ -> raise (Foo "unimplemented")

and type_dec ((tenv, venv) as env) dec : (E.tyenv * E.venv) * Tpd.typed_dec =
  match dec with
  | A.VarDec { name; typ; init; pos } ->
      let tinit, typed_init = type_exp env init in
      let tvar, typed_typ =
        match typ with
        | Some (tname, pos) ->
            (* Check if typ and tinit matches *)
            let tsym = S.symbol tname in
            let t = tylookup tsym tenv in
            let tinfo : Syminfo.info =
              {
                sym = tsym;
                kind = Syminfo.TYPE;
                tpe = t;
                role = Syminfo.REF;
                level = 0;
              }
            in
            coerce tinit t;
            (t, Some (tinfo, pos))
        | None ->
            if T.coerceable tinit T.NIL then raise (Foo "cannot be nil")
            else (tinit, None)
      in
      let sym = S.symbol name in
      let entry = E.VarEntry { ty = tvar; const = false } in
      let venv' = Symtab.enter sym entry venv in
      let info : Syminfo.info =
        { sym; kind = Syminfo.VAR; tpe = tvar; role = Syminfo.DEF; level = 0 }
      in
      let typed_vardec =
        Tpd.TypedVarDec { name = info; typ = typed_typ; init = typed_init; pos }
      in
      ((tenv, venv'), typed_vardec)
  | _ -> raise (Foo "unimplemented")

(* TODO: return type tree *)
let type_prog (tree : A.exp) : Tpd.typed_exp =
  let _, tpd_tree = type_exp (Env.base_tenv, Env.base_venv) tree in
  tpd_tree
