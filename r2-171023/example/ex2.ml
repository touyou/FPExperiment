type iexpr =
  | EConstInt of int
  | EAdd of iexpr * iexpr
  | ESub of iexpr * iexpr
  | EMul of iexpr * iexpr;;

let rec eval ex =
  match ex with
  | EConstInt i -> i
  | EAdd (a, b) -> eval a + eval b
  | ESub (a, b) -> eval a - eval b
  | EMul (a, b) -> eval a * eval b;;
