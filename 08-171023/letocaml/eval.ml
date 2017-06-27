open Syntax
open TySyntax
open ConstraintSolver

exception Unbound
exception EvalUnbound
exception EvalType
exception EvalZeroDivision

type tyenv = (name * type_schema) list

let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

(* get_type_vars : ty -> tyvar list *)
let rec get_type_vars typ =
  match typ with
  | TyVar x -> [x]
  | TyFun (v1, v2) -> (get_type_vars v1) @ (get_type_vars v2)
  | _ -> []
;;

let rec check_env env typ =
  match env with
  | [] -> true
  | (n, t) :: envr -> let (tlist, tp) = t in if tp = typ then false else check_env envr typ
;;

(* generalize : tyenv -> ty -> type_schema *)
let rec generalize env typ =
  match typ with
  | TyInt -> ([], TyInt)
  | TyBool -> ([], TyBool)
  | TyVar x -> if check_env env typ then ([x], typ) else ([], typ)
  | TyFun (v1, v2) ->
      let (l1, t1) = generalize env v1 in
      let (l2, t2) = generalize env v2 in
      (l1@l2, typ)
;;

let rec ex_tvar typ v newv =
  match typ with
  | TyVar x -> if x=v then TyVar newv else typ
  | TyFun (x1, x2) -> TyFun ((ex_tvar x1 v newv), (ex_tvar x2 v newv))
  | _ -> typ
;;

(* instantiate : type_schema -> ty *)
let rec instantiate scm =
  match scm with
  | ([], typ) -> typ
  | (v :: vr, typ) ->
      let newv = new_tyvar () in
      instantiate (vr, ex_tvar typ v newv)
;;

(* tlistに入ってないサブストだけを実行する  *)
let rec tyscheme_subst s t =
  let (tlist, typ) = t in
  let news = List.filter (fun (tvar, typ) -> not (List.exists (fun x -> x = tvar) tlist)) s in
  match typ with
  | TyInt -> (tlist, typ)
  | TyBool -> (tlist, typ)
  | TyFun (x, y) -> (tlist, TyFun (ty_subst news x, ty_subst news y))
  | TyVar x ->
    try
      (tlist, lookup_subst news x)
    with
    | SubstUnbound -> (tlist, TyVar x)
;;

(* tyenv_subst subst -> tyenv -> tyenv *)
let tyenv_subst s t =
  List.map (fun (x, typ) -> (x, tyscheme_subst s typ)) t
;;

(* infer_expr: tyenv->expr->ty * constraints *)
let rec infer_expr env e =
  match e with
  | EConstInt i -> (TyInt, [])
  | EConstBool b -> (TyBool, [])
  | EVar x ->
    (try
      let tsc = lookup x env in
       (instantiate tsc, [])
     with
     | Unbound -> raise EvalUnbound)
  | ELet (x, e1, e2) ->
    let (t1, c1) = infer_expr env e1 in
    let sigma = unify c1 in
    let s1 = ty_subst sigma t1 in
    let delta = tyenv_subst sigma env in
    let p1 = get_type_vars s1 in
    let p2 = List.concat (List.map (fun (lis, typ) -> let (tlist, _) = typ in tlist) delta) in
    let (t2, c2) = infer_expr ((x, (p1@p2, s1)) :: env) e2 in
    (t2, c2 @ c1)
  | EIf (e1, e2, e3) ->
    let (t1, c1) = infer_expr env e1 in
    let (t2, c2) = infer_expr env e2 in
    let (t3, c3) = infer_expr env e3 in
    (t2, [(t1,TyBool); (t2, t3)] @ c1 @ c2 @ c3)
  | EFun (x, e) ->
    let alpha = TyVar (new_tyvar ()) in
    let env' = (x, generalize env alpha) :: env in
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
    let (t1, c1) = infer_expr ((f, generalize env (TyFun (alpha, beta))) :: ((x, generalize env alpha) :: env)) e1 in
    let (t2, c2) = infer_expr ((f, generalize env (TyFun (alpha, beta))) :: env) e2 in
    (t2, (t1, beta) :: c1 @ c2)
  | EAdd (e1, e2) | ESub (e1, e2) | EMul (e1, e2) | EDiv (e1, e2) ->
    let (t1, c1) = infer_expr env e1 in
    let (t2, c2) = infer_expr env e2 in
    (TyInt, [(t1, TyInt); (t2, TyInt)] @ c1 @ c2)
  | EEq (e1, e2) | ELt (e1, e2) ->
    let (t1, c1) = infer_expr env e1 in
    let (t2, c2) = infer_expr env e2 in
    (TyBool, [(t1, TyInt); (t2, TyInt)] @ c1 @ c2)
  | EAnd (e1, e2) | EOr (e1, e2) ->
    let (t1, c1) = infer_expr env e1 in
    let (t2, c2) = infer_expr env e2 in
    (TyBool, [(t1, TyBool); (t2, TyBool)] @ c1 @ c2)

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
      (t', (x, generalize env t') :: env)
    | CRecDecl (f, x, e) ->
      let alpha = TyVar (new_tyvar ()) in
      let beta = TyVar (new_tyvar ()) in
      let (t, c) = infer_expr ((f, generalize env (TyFun (alpha, beta))) :: (x, generalize env alpha) :: env) e in
      let t' = ty_subst (unify c) t in
      (t', (f, generalize env t') :: env)
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
