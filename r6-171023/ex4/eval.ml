open Syntax

exception Unbound
exception EvalUnbound
exception EvalType
exception EvalZeroDivision

type value =
  | VInt  of int
  | VBool of bool
  | VFun of name * expr * env ref
  | VError of string
and env = (name * value) list

let print_value v =
  match v with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun (x, e, env) -> print_string "VFun("; print_name x; print_string ")"
  | VError s -> print_string "Error: "; print_string s

let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

let rec eval_expr env e =
  match e with
  | EConstInt i -> VInt i
  | EConstBool b -> VBool b
  | EVar x ->
      (try
        lookup x env
      with
      | Unbound -> raise EvalUnbound)
  | EFun (x, e) -> VFun (x, e, ref env)
  | ELet (x,e1,e2) ->
      let v1 = eval_expr env e1 in
      eval_expr (extend x v1 env) e2
  | ELetRec (f, x, e1, e2) ->
      let oenv = ref [] in
      let v = VFun(x, e1, oenv) in
        (oenv := extend f v env; eval_expr (extend f v env) e2)
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
  | EIf (e1,e2,e3) ->
      let v1 = eval_expr env e1 in
      (match v1 with
      | VBool b -> if b then eval_expr env e2 else eval_expr env e3
      | _ -> raise EvalType)
  | EApp (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match v1 with
      | VFun (x, e, oenv) -> eval_expr (extend x v2 (!oenv)) e
      | _ -> raise EvalType)


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
  | CRecDecl (f, x, e) ->
    (try
      let oenv = ref [] in
      let v = VFun(x, e, oenv) in
        (oenv := extend f v env;
          let newenv = extend f v env in
          (f, newenv, v))
    with
    | EvalType -> ("-", env, VError "Type Mismatching")
    | EvalUnbound -> ("-", env, VError "Unbounded Value")
    | EvalZeroDivision -> ("-", env, VError "Divided by Zero"))
