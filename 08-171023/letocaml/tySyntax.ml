
type tyvar = int

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar

type type_schema = tyvar list * ty

let tvar = ref 0;;

let new_tyvar a = tvar := !tvar + 1; !tvar;;


