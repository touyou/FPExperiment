open Syntax
open Eval
open TySyntax
open ConstraintSolver

let rec read_eval_print env tenv =
  print_string "# ";
  flush stdout;
  (try
    let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    (try
      let (t, newtenv) = infer_cmd tenv cmd in
      let (id, newenv, v) = eval_command env cmd in
      (Printf.printf "%s : " id;
       print_type t;
       print_string " = ";
       print_value v;
       print_newline ();
       read_eval_print newenv newtenv)
    with
    | EvalUnbound ->
      Printf.printf "Error: Unbounded value.";
      print_newline ();
      read_eval_print env tenv
    | EvalType ->
      Printf.printf "Error: Type Missmatch.";
      print_newline ();
      read_eval_print env tenv
    | EvalZeroDivision ->
      Printf.printf "Exception: Division_by_zero.";
      print_newline ();
      read_eval_print env tenv
    | Unbound ->
      Printf.printf "Type Error.";
      print_newline ();
      read_eval_print env tenv)
  with
  | Parsing.Parse_error ->
    Printf.printf "Error: Parse failed.";
    print_newline ();
    read_eval_print env tenv
  | Lexer.Unknown ->
    Printf.printf "Error: Unknown token";
    print_newline ();
    read_eval_print env tenv)

let initial_env =
  extend "i" (VInt 1)
	 (extend "v" (VInt 5)
		 (extend "x" (VInt 10)
			 empty_env))

let _ = read_eval_print initial_env []
