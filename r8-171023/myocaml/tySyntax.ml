open ConstraintSolver

exception SubstUnbound

type tyvar = string

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar

(* subst lookup *)
let rec lookup_subst s v =
  match s with
  | [] -> raise SubstUnbound
  | (tv, typ) :: sr -> if tv = v then typ else lookup_subst sr v;;

(* ty_subst : subst -> ty -> ty *)
let rec ty_subst s t =
  match t with
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (x, y) -> TyFun (ty_subst s x, ty_subst s y)
  | TyVar x ->
    try
      lookup_subst s x
    with
    | SubstUnbound -> TyVar x;;

(* compose : subst -> subst -> subst *)
let compose s1 s2 = (*  *)
