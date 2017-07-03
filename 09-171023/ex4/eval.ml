open Syntax
open TySyntax
open ConstraintSolver

exception Unbound
exception EvalUnbound
exception EvalType
exception EvalZeroDivision

let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

  (* infer *)
  let rec infer_expr env e =
    match e with
    | EConstInt i -> (TyInt, [])
    | EConstBool b -> (TyBool, [])
    | EVar x ->
      (try
        (lookup x env, [])
      with
      | Unbound -> raise EvalUnbound)
    | EAdd (e1, e2)
    | ESub (e1, e2)
    | EMul (e1, e2)
    | EDiv (e1, e2) ->
      let (t1, c1) = infer_expr env e1 in
      let (t2, c2) = infer_expr env e2 in
      (TyInt, [(t1, TyInt); (t2, TyInt)] @ c1 @ c2)
    | EEq (e1, e2)
    | ELt (e1, e2) ->
      let (t1, c1) = infer_expr env e1 in
      let (t2, c2) = infer_expr env e2 in
      (TyBool, c1 @ c2)
    | EIf (e1, e2, e3) ->
      let (t1, c1) = infer_expr env e1 in
      let (t2, c2) = infer_expr env e2 in
      let (t3, c3) = infer_expr env e3 in
      (t2, [(t1,TyBool); (t2, t3)] @ c1 @ c2 @ c3)
    | ELet (x, e1, e2) ->
      let (t1, c1) = infer_expr env e1 in
      let env' = (x, t1) :: env in
      let (t2, c2) = infer_expr env' e2 in
      (t2, c1 @ c2)
    | EFun (x, e) ->
      let alpha = TyVar (new_tyvar ()) in
      let env' = (x, alpha) :: env in
      let (t, c) = infer_expr env' e in
      (TyFun (alpha, t), c)
    | EApp (e1, e2) ->
      let (t1, c1) = infer_expr env e1 in
      let (t2, c2) = infer_expr env e2 in
      let alpha = TyVar (new_tyvar ()) in
      (alpha, ((t1, TyFun (t2, alpha)) :: c1) @ c2)
    | ELetRec (f, x, e1, e2) ->
      let alpha = TyVar (new_tyvar ()) in
      let beta = TyVar (new_tyvar ()) in
      let (t1, c1) = infer_expr ((f, TyFun (alpha, beta)) :: ((x, alpha) :: env)) e1 in
      let (t2, c2) = infer_expr ((f, TyFun (alpha, beta)) :: env) e2 in
      (t2, (t1, beta) :: c1 @ c2)
    | EPair (e1, e2) ->
      let (t1, c1) = infer_expr env e1 in
      let (t2, c2) = infer_expr env e2 in
      (TyPair (t1, t2), c1 @ c2)
    | ENil ->
      (TyList [], [])
    | ECons (e1, e2) ->
      let (t1, c1) = infer_expr env e1 in
      let (t2, c2) = infer_expr env e2 in
      (TyList [t1; t2], (t2, t1) :: c1 @ c2)
    | ERecFun (f, x, e, oenv) ->
      let alpha = TyVar (new_tyvar ()) in
      let beta = TyVar (new_tyvar ()) in
      infer_expr ((f, TyFun (alpha, beta)) :: (x, alpha) :: env) e

  let infer_cmd env c =
    try
      match c with
      | CExp e ->
        let (t, c) = infer_expr env e in
        let t' = ty_subst (unify c) t in
        (t', env)
      | CDecl (x, e) ->
        let (t, c) = infer_expr env e in
        let t' = ty_subst (unify c) t in
        (t', (x, t') :: env)
      | CRecDecl (f, x, e) ->
        let alpha = TyVar (new_tyvar ()) in
        let beta = TyVar (new_tyvar ()) in
        let (t, c) = infer_expr ((f, TyFun (alpha, beta)) :: (x, alpha) :: env) e in
        let t' = ty_subst (unify c) t in
        let xt = ty_subst (unify c) alpha in
        (TyFun (xt, t'), (f, TyFun(alpha, t')) :: env)
    with
    | TyError -> raise Unbound
    | EvalUnbound -> raise Unbound

let rec eval_expr env e =
  match e with
  | EConstInt i -> VInt i
  | EConstBool b -> VBool b
  | EVar x ->
    (try
      let Thunk (e, env') = lookup x env in
      eval_expr env' e
    with
    | Unbound -> raise EvalUnbound)
  | EAdd (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
    | VInt i1, VInt i2 -> VInt (i1 + i2)
    | _ -> raise EvalType)
  | ESub (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
    | VInt i1, VInt i2 -> VInt (i1 - i2)
    | _ -> raise EvalType)
  | EMul (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
    | VInt i1, VInt i2 -> VInt (i1 * i2)
    | _ -> raise EvalType)
  | EDiv (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
    | VInt i1, VInt i2 -> if i2 = 0 then raise EvalZeroDivision else VInt (i1 / i2)
    | _ -> raise EvalType)
  | EEq (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
    | VInt i1, VInt i2 -> VBool (i1 = i2)
    | VBool b1, VBool b2 -> VBool (b1 = b2)
    | _ -> raise EvalType)
  | ELt (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
    | VInt i1, VInt i2 -> VBool (i1 < i2)
    | VBool b1, VBool b2 -> VBool (b1 < b2)
    | _ -> raise EvalType)
  | EIf (e1, e2, e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
    | VBool b -> if b then eval_expr env e2 else eval_expr env e3
    | _ -> raise EvalType)
  | ELet (x, e1, e2) ->
    let th = Thunk (e1, env) in
    eval_expr (extend x th env) e2
  | EFun (x, e) -> VFun (x, e, env)
  | EApp (e1, e2) ->
    let v1 = eval_expr env e1 in
    let th = Thunk (e2, env) in
    (match v1 with
    | VFun (x, e, env') -> eval_expr (extend x th env') e
    | VRecFun (f, x, e, oenv) ->
      let th2 = Thunk (ERecFun (f, x, e, oenv), oenv) in
      let env' = extend x th (extend f th2 oenv) in
      eval_expr env' e
      (*let env' = extend x th (extend f (VRecFun (f, x, e, oenv)) oenv) in
      eval_expr env' e*)
    | _ -> raise EvalUnbound)
  | ELetRec (f, x, e1, e2) ->
    let th = Thunk (ERecFun (f, x, e1, env), env) in
    let env' = extend f th env in
    (*let env' = extend f (VRecFun (f, x, e1, env)) env in*)
    eval_expr env' e2
  | ERecFun (f, x, e, env) -> VRecFun (f, x, e, env)
  | EPair (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VPair (v1, v2)
  | ENil -> VNil
  | ECons (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VCons (v1, v2)

let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (x, e) ->
    let v = eval_expr env e in
    let th = Thunk (e, env) in
    let env' = extend x th env in
    (x, env', v)
  | CRecDecl (f, x, e) ->
    let v = VRecFun (f, x, e, env) in
    let th = Thunk (ERecFun (f, x, e, env), env) in
    let env' = extend f th env in
    (f, env', v)
    (*let v = VRecFun (f, x, e, env) in
    let env' = extend f v env in
    (f, env', v)*)