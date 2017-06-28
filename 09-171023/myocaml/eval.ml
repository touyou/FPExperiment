open Syntax
open TySyntax
open ConstraintSolver

exception Unbound
exception EvalUnbound
exception EvalType
exception EvalZeroDivision

type tyenv = (name * ty) list

let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

(* find_match : pattern -> value -> (name * value) list option *)
let rec find_match p v =
  match p, v with
  | (PInt x, VInt y) -> if x=y then Some [] else None
  | (PBool x, VBool y) -> if x=y then Some [] else None
  | (PVar n, m) -> Some [(n, m)]
  | (PPair (x1,y1), VPair (x2,y2)) ->
      match find_match x1 x2, find_match x2 y2 with
      | (Some r1, Some r2) -> Some r1@r2
      | (_, _) -> None		
  | (PCons (x1,y1), VCons (x2,y2)) ->
      match find_match x1 x2, find_match x2 y2 with
      | (Some r1, Some r2) -> Some r1@r2
      | (_, _) -> None
  | (PNil, VNil) -> Some []
  | (_, _) -> None

(* infer_expr: tyenv->expr->ty * constraints *)
let rec infer_expr env e =
  match e with
  | EConstInt i -> (TyInt, [])
  | EConstBool b -> (TyBool, [])
  | EVar x ->
    (try
       (lookup x env, [])
     with
     | Unbound -> raise EvalUnbound)
  | ELet (x, e1, e2) ->
    let (t1, c1) = infer_expr env e1 in
    let env' = (x, t1) :: env in
    let (t2, c2) = infer_expr env' e2 in
    (t2, c1 @ c2)
  | EIf (e1, e2, e3) ->
    let (t1, c1) = infer_expr env e1 in
    let (t2, c2) = infer_expr env e2 in
    let (t3, c3) = infer_expr env e3 in
    (t2, [(t1,TyBool); (t2, t3)] @ c1 @ c2 @ c3)
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
    (TyBool, [(t1, TyInt); (t2, TyInt)] @ c1 @ c2)
  | EAnd (e1, e2)
  | EOr (e1, e2) ->
    let (t1, c1) = infer_expr env e1 in
    let (t2, c2) = infer_expr env e2 in
    (TyBool, [(t1, TyBool); (t2, TyBool)] @ c1 @ c2)
  | EMatch (e, p) ->
    let (t, c) = infer_expr env e in


(* infer_cmd: tyenv->cmd->ty*tyenv *)
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
      (t', (f, t') :: env)
    | CQuit -> (TyInt, env)
  with
  | TyError -> raise Unbound
  | EvalUnbound -> raise Unbound

let rec eval_expr env e =
  match e with
  | EConstInt i -> VInt i
  | EConstBool b -> VBool b
  | EVar x ->
    (try
       lookup x env
     with
     | Unbound -> raise EvalUnbound)
  | EFun (x, e) -> VFun (x, e, env)
  | ELet (x,e1,e2) ->
    let v1 = eval_expr env e1 in
    eval_expr (extend x v1 env) e2
  | ELetRec (f,x,e1,e2) ->
    let env' = extend f (VRecFun(f,x,e1,env)) env in
    eval_expr env' e2
  | EAdd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 + i2)
     | _ -> raise EvalType)
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> raise EvalType)
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> raise EvalType)
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> if i2 = 0 then raise EvalZeroDivision else VInt (i1 / i2)
     | VError e, _ -> VError e
     | _ -> raise EvalType)
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VBool (i1 = i2)
     | _ -> raise EvalType)
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VBool (i1 < i2)
     | _ -> raise EvalType)
  | EAnd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VBool b1, VBool b2 -> VBool (b1 && b2)
     | _ -> raise EvalType)
  | EOr (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VBool b1, VBool b2 -> VBool (b1 || b2)
     | _ -> raise EvalType)
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | VBool b -> if b then eval_expr env e2 else eval_expr env e3
     | _ -> raise EvalType)
  | EApp (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
     | VFun(x,e,oenv) ->
       eval_expr (extend x v2 oenv) e
     | VRecFun(f,x,e,oenv) ->
       let env' = extend x v2 (extend f (VRecFun(f,x,e,oenv)) oenv) in
       eval_expr env' e
     | _ -> raise EvalUnbound)
  | EPair (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VPair (v1, v2)
  | ENil -> VNil
  | ECons (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VCons (v1, v2)
  | EMatch (e1, p) ->
    let v = eval_expr env e in
    lookup_match p v env
(* lookup_match: (pattern * expr) list -> value -> env ->  *)
and lookup_match p v env =
  match p with
  | [] -> raise EvalType
  | (pat, exp) :: pr ->
    let ret = find_match pat v in
    if ret = None then lookup_match pr else
      let Some e = ret in
      let env' = e @ env in
      eval_expr env' exp


let rec eval_command env c =
  match c with
  | CExp e ->
    (try
       ("-", env, eval_expr env e)
     with
     | EvalType -> ("-", env, VError "Type Mismatching")
     | EvalUnbound -> ("-", env, VError "Unbounded Value")
     | EvalZeroDivision -> ("-", env, VError "Divided by Zero"))
  | CDecl (x,e) ->
    (try
       let v = eval_expr env e in
       let newenv = extend x v env in
       (x, newenv, v)
     with
     | EvalType -> ("-", env, VError "Type Mismatching")
     | EvalUnbound -> ("-", env, VError "Unbounded Value")
     | EvalZeroDivision -> ("-", env, VError "Divided by Zero"))
  | CRecDecl (f,x,e) ->
    (try
       let v = VRecFun(f,x,e,env) in
       let newenv = extend f v env in
       (f, newenv, v)
     with
     | EvalType -> ("-", env, VError "Type Mismatching")
     | EvalUnbound -> ("-", env, VError "Unbounded Value")
     | EvalZeroDivision -> ("-", env, VError "Divided by Zero"))
  | CQuit -> exit 0
