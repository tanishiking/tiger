module A = Core.Ast
module S = Core.Symbol
module T = Core.Types
module E = Env
module Tpd = Core.Tpd
module Syminfo = Core.Syminfo
module Error = Core.Error

let type_mismatch loc expected found =
  Error.error loc "type mismatch: expected %s, found %s" (T.show_tpe expected)
    (T.show_tpe found)

let undefined loc kind id = Error.error loc "undefined %s %s" kind (S.name id)

let misdefined loc kind id = Error.error loc "%s is not a %s" (S.name id) kind

let cannot_be_nil loc id =
  Error.error loc "cannot initialize untyped variable %s with nil" (S.name id)

let coerce ty1 ty2 pos =
  if not (T.coerceable ty1 ty2) then type_mismatch pos ty2 ty1

let lookup id env pos kind =
  match Symtab.lookup id env with Some x -> x | None -> undefined pos kind id

let vlookup id venv pos : E.enventry = lookup id venv pos "variable"

let tylookup id tyenv pos : T.tpe = lookup id tyenv pos "type"

let type_var (_, venv, var) : T.tpe * Tpd.typed_var =
  match var with
  | A.SimpleVar (name, pos) -> (
      let sym = S.symbol name in
      match vlookup sym venv pos with
      | E.FunEntry _ -> misdefined pos "variable" sym
      | E.VarEntry { ty; _ } ->
          let info : Syminfo.info =
            { sym; kind = Syminfo.VAR; tpe = ty; role = Syminfo.REF; level = 0 }
          in
          (T.actual_ty ty, Tpd.TypedSimpleVar (info, pos)) )
  | _ -> Error.fatal "not implemented"

let rec type_exp (((tenv : E.tyenv), (venv : E.venv)) as env) exp :
    T.tpe * Tpd.typed_exp =
  match exp with
  | A.NilExp -> (T.NIL, Tpd.TypedNilExp)
  | A.IntExp i -> (T.INT, Tpd.TypedIntExp i)
  | A.StringExp (str, p) -> (T.STRING, Tpd.TypedStringExp (str, p))
  | A.SeqExp exps ->
      let ty, typed_exps =
        List.fold_left
          (fun (_, typed_exps_pos) (exp, pos) ->
            let t, typed_exp = type_exp env exp in
            (t, typed_exps_pos @ [ (typed_exp, pos) ]))
          (T.UNIT, []) exps
      in
      (ty, TypedSeqExp typed_exps)
  | A.VarExp v ->
      let tvar, typed_var = type_var (tenv, venv, v) in
      (tvar, TypedVarExp typed_var)
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
  | _ -> Error.fatal "not implemented"

and type_dec ((tenv, venv) as env) dec : (E.tyenv * E.venv) * Tpd.typed_dec =
  match dec with
  | A.VarDec { name; typ; init; pos } ->
      let tinit, typed_init = type_exp env init in
      let varsym = S.symbol name in
      let tvar, typed_typ =
        match typ with
        | Some (tname, pos) ->
            (* Check if typ and tinit matches *)
            let tsym = S.symbol tname in
            let t = tylookup tsym tenv pos in
            let tinfo : Syminfo.info =
              {
                sym = tsym;
                kind = Syminfo.TYPE;
                tpe = t;
                role = Syminfo.REF;
                level = 0;
              }
            in
            coerce tinit t pos;
            (t, Some (tinfo, pos))
        | None ->
            if T.coerceable tinit T.NIL then cannot_be_nil pos varsym
            else (tinit, None)
      in
      let entry = E.VarEntry { ty = tvar; const = false } in
      let venv' = Symtab.enter varsym entry venv in
      let info : Syminfo.info =
        {
          sym = varsym;
          kind = Syminfo.VAR;
          tpe = tvar;
          role = Syminfo.DEF;
          level = 0;
        }
      in
      let typed_vardec =
        Tpd.TypedVarDec { name = info; typ = typed_typ; init = typed_init; pos }
      in
      ((tenv, venv'), typed_vardec)
  | _ -> Error.fatal "unimplemented"

(* TODO: return type tree *)
let type_prog (tree : A.exp) : Tpd.typed_exp =
  let _, tpd_tree = type_exp (Env.base_tenv, Env.base_venv) tree in
  tpd_tree
