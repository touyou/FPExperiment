type name = string

type pattern =
  | PInt of int
  | PBool of bool
  | PVar of name
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern

type expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name
  | EFun       of name * expr
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr
  | EAnd       of expr * expr
  | EOr        of expr * expr
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ELetRec    of name * name * expr * expr
  | EApp       of expr * expr
  | EPair      of expr * expr
  | ENil
  | ECons      of expr * expr
  | EMatch     of expr * (pattern * expr) list

type value =
  | VInt  of int
  | VBool of bool
  | VFun of name * expr * env
  | VRecFun of name * name * expr * env
  | VError of string
  | VPair of value * value
  | VNil
  | VCons of value * value
and env = (name * value) list

type command =
  | CExp  of expr
  | CDecl of name * expr
  | CRecDecl of name * name * expr
  | CQuit

let print_name = print_string

let rec print_pattern p =
  match p with
  | PInt i -> print_int i
  | PBool b -> print_string (string_of_bool b)
  | PVar x -> print_string x
  | PPair (p1, p2) ->
     (print_string "(";
      print_pattern p1;
      print_string ",";
      print_pattern p2;
      print_string ")")
  | PNil -> print_string "[]"
  | PCons (p1, p2) ->
     (print_pattern p1;
      print_string "::";
      print_pattern p2)

let rec print_value v =
  match v with
  | VInt i  -> print_string "VInt("; print_int i; print_string ")"
  | VBool b -> print_string "VBool("; print_string (string_of_bool b); print_string ")"
  | VFun (x, e, env) -> print_string "VFun("; print_name x; print_string ")="; print_expr e
  | VRecFun (f, x, e, env) -> print_string "VRecFun("; print_name f; print_string ","; print_name x; print_string ")="; print_expr e
  | VError s -> print_string "Error: "; print_string s
  | VPair (v1, v2) -> print_string "VPair("; print_value v1; print_string ","; pring_value v2; print_string")"
and print_expr e =
  match e with
  | EConstInt i ->
     print_int i
  | EConstBool b ->
     print_string (string_of_bool b)
  | EVar x ->
     print_name x
  | ELet (x,e1,e2) ->
     (print_string "ELet (";
      print_string x;
      print_string ",";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string")")
  | ELetRec (f,x,e1,e2) ->
     (print_string "ELet (";
      print_string f;
      print_string ",";
      print_string x;
      print_string ",";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string")")
  | EFun (x,e) ->
     (print_string "EFun (";
      print_string x;
      print_string ",";
      print_expr e;
      print_string")")
  | EAdd (e1,e2) ->
     (print_string "EAdd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ESub (e1,e2) ->
     (print_string "ESub (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EMul (e1,e2) ->
     (print_string "EMul (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EDiv (e1,e2) ->
     (print_string "EDiv (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EEq (e1,e2) ->
     (print_string "EEq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ELt (e1, e2) ->
     (print_string "ELt (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EAnd (e1, e2) ->
      (print_string "EAnd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EOr (e1, e2) ->
      (print_string "EOr (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EIf (e1,e2,e3) ->
     (print_string "EIf (";
      print_expr   e1;
      print_string ",";
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")")
  | EApp (e1, e2) ->
      (print_string "EApp (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EPair (e1, e2) ->
     (print_string "EPair (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ENil ->
     print_string "ENil"
  | ECons (e1, e2) ->
     (print_string "ECons (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
| EMatch (e, cases) ->
     (print_string "EMatch (";
      print_expr e;
      print_string ",";
      print_cases cases;
      print_string ")")
and print_cases cases =
  List.iter (fun (p, e) ->
	     print_pattern p;
	     print_string " -> ";
	     print_expr e;
	     print_string ",")
	    cases

let rec print_command p =
  match p with
  | CExp e -> print_expr e
  | CDecl (x, e) ->
      (print_string x;
       print_string "=";
       print_expr e)
  | CRecDecl (f, x, e) ->
      (print_string f;
       print_string " ";
       print_string x;
       print_string "=";
       print_expr e)
  | CQuit -> print_string "Quit"
