open TySyntax

exception TyError
exception SubstUnbound

type subst = (tyvar * ty) list
type constraints = (ty * ty) list

(* subst lookup *)
let rec lookup_subst s v =
  match s with
  | [] -> raise SubstUnbound
  | (tv, typ) :: sr -> if tv = v then typ else lookup_subst sr v
;;

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
    | SubstUnbound -> TyVar x
;;

(* compose : subst -> subst -> subst *)
let rec compose s1 s2 =
  match s1 with
  | [] -> s2
  | (tv, typ) :: sr -> (tv, ty_subst s2 typ) :: compose sr s2
;;

let rec subst_con s c =
  match c with
  | [] -> []
  | (t1, t2) :: cr -> (ty_subst s t1, ty_subst s t2) :: subst_con s cr
;;

(* ty_unify : constraints -> subst *)
let rec unify c=
  match c with
  | [] -> []
  | (t1, t2) :: cr ->
      if t1 = t2 then
        unify cr
      else
        match t1, t2 with
        | (TyFun (s1, r1), TyFun (s2, r2)) -> unify ((s1, s2)::(r1,r2)::cr)
        | (TyVar x, t) -> if (ty_subst ((x, t)::[]) t) = t then (x, t) :: (unify (subst_con ((x, t)::[]) cr)) else raise TyError
        | (t, TyVar x) -> if (ty_subst ((x, t)::[]) t) = t then (x, t) :: (unify (subst_con ((x, t)::[]) cr)) else raise TyError
        | (_, _) -> raise TyError
;;
