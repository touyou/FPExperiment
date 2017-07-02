
type tyvar = int

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyPair of ty * ty
  | TyList of ty list

let tvar = ref 0;;

let new_tyvar a = tvar := !tvar + 1; !tvar;;

let rec print_type t =
  match t with
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyFun (t1, t2) -> print_type t1; print_string " -> "; print_type t2
  | TyVar v -> Printf.printf "a%d" v
  | TyPair (t1, t2) -> print_type t1; print_string " * "; print_type t2
  | TyList [] -> print_string "'a list"
  | TyList (t1 :: tr) -> print_type t1; print_string " list"
;;