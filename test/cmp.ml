open Syntax.Ast

(* cmp function for tests, ignore position and  *)
let rec cmp_exp e1 e2 =
  match (e1, e2) with
  | NilExp, NilExp -> true
  | IntExp i1, IntExp i2 -> i1 = i2
  | StringExp (s1, _), StringExp (s2, _) -> s1 = s2
  | VarExp v1, VarExp v2 -> cmp_var v1 v2
  | ( AssignExp { var = v1; exp = e1; pos = _ },
      AssignExp { var = v2; exp = e2; pos = _ } ) ->
      cmp_var v1 v2 && cmp_exp e1 e2
  | _ -> false

and cmp_var v1 v2 =
  match (v1, v2) with
  | SimpleVar (s1, _), SimpleVar (s2, _) -> cmp_sym s1 s2
  | FieldVar (v1, s1, _), FieldVar (v2, s2, _) -> cmp_var v1 v2 && cmp_sym s1 s2
  | SubscriptVar (v1, e1, _), SubscriptVar (v2, e2, _) ->
      cmp_var v1 v2 && cmp_exp e1 e2
  | _ -> false

and cmp_sym s1 s2 =
  match (s1, s2) with (name1, _), (name2, _) -> name1 = name2
