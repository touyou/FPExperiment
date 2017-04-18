type value = VInt of int | VBool of bool;;
type expr =
  | EConstInt of int
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | EConstBool of bool
  | EEq of expr * expr
  | ELt of expr * expr
  | EIfel of expr * expr * expr;;
exception Eval_error;;

let bool_of_value v =
  match v with
  | VBool b -> b
  | VInt num -> raise Eval_error;;

let num_of_value v =
  match v with
  | VInt num -> num
  | VBool b -> raise Eval_error;;

let rec eval exp =
  match exp with
  | EConstInt n -> VInt n
  | EAdd (a, b) -> num_of_value (eval a) + num_of_value (eval b)
  | ESub (a, b) -> num_of_value (eval a) - num_of_value (eval b)
  | EMul (a, b) -> num_of_value (eval a) * num_of_value (eval b)
  | EDiv (a, b) -> if num_of_value (eval b) = 0 then raise Eval_error else num_of_value (eval a) - num_of_value (eval b)
  | EConstBool n -> VBool n
  | EEq (a, b) -> 
  | ELt (a, b) ->
  | EIfel (a, b, c) -> if (eval a) then (eval b) else (eval c);;
