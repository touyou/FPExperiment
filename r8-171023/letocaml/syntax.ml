type name = string

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

type value =
  | VInt  of int
  | VBool of bool
  | VFun of name * expr * env
  | VRecFun of name * name * expr * env
  | VError of string
and env = (name * value) list

type command =
  | CExp  of expr
  | CDecl of name * expr
  | CRecDecl of name * name * expr
  | CQuit

let print_name = print_string

let print_value v =
  match v with
  | VInt i  -> print_string "VInt("; print_int i; print_string ")"
  | VBool b -> print_string "VBool("; print_string (string_of_bool b); print_string ")"
  | VFun (x, e, env) -> print_string "VFun("; print_name x; print_string ")"
  | VRecFun (f, x, e, env) -> print_string "VRecFun("; print_name f; print_string ","; print_name x; print_string ")"
  | VError s -> print_string "Error: "; print_string s
let rec print_expr e =
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
