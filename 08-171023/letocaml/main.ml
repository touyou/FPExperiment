open Syntax
open Eval

let rec read_eval_print env tenv =
  print_string "# ";
  flush stdout;
  try
    let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (t, tyenv) = infer_cmd tenv cmd in
    let (id, newenv, v) = eval_command env cmd in
    (Printf.printf "%s = " id;
     print_value v;
     print_newline ();
     read_eval_print newenv tyenv)
  with
  | Parsing.Parse_error ->
    (Printf.printf "Error = Unknown Syntax";
     print_newline ();
     read_eval_print env tenv)
  | Lexer.Unknown ->
    (Printf.printf "Error = Unknown Syntax";
     print_newline ();
     read_eval_print env tenv)
  | Unbound ->
    (Printf.printf "Error = Invalid Type";
     print_newline ();
     read_eval_print env tenv)
  | ConstraintSolver.TyError ->
    (Printf.printf "Error = Invalid Type";
     print_newline ();
     read_eval_print env tenv)

let initial_env = empty_env

let _ = read_eval_print initial_env []
