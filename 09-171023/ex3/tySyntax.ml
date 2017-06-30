
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


