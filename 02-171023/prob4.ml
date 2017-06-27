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
