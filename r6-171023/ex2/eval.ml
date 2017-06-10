open Syntax

exception Unbound
exception EvalUnbound
exception EvalType
exception EvalZeroDivision

let empty_env = []
let extend x v env = (x, v) :: env
let rec extend_list oe e oenv env n =
  match e with
  | y :: yr ->
    let (f, x, v) = y in
    extend_list oe yr oenv ((f, VRecFun (n, oe, oenv)) :: env) (n+1)
  | [] -> env;;

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

let rec get_n el n =
  match el with
  | e :: ex -> if n=1 then e else get_n ex (n-1)
  | [] -> raise EvalUnbound

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
  | ELetRec (f,e) ->
      let env' = extend_list f f env env 1 in
      eval_expr env' e
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
      | VRecFun(n, e, oenv) ->
          let (fi, xi, ei) = get_n e n in
          eval_expr (extend xi v2 env) ei
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
  | CRecDecl e ->
    let newenv = extend_list e e env env 1 in
    ("-", newenv, VRecFun(1,e,env))
  | CQuit -> exit 0
