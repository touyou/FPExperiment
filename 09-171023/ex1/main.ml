open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  (try
    let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    (try
      let (id, newenv, v) = eval_command env cmd in
      (Printf.printf "%s : " id;
       print_string " = ";
       print_value v;
       print_newline ();
       read_eval_print newenv)
    with
    | EvalUnbound ->
      Printf.printf "Error: Unbounded value.";
      print_newline ();
      read_eval_print env
    | EvalType ->
      Printf.printf "Error: Type Missmatch.";
      print_newline ();
      read_eval_print env
    | EvalZeroDivision ->
      Printf.printf "Exception: Division_by_zero.";
      print_newline ();
      read_eval_print env)
  with
  | Parsing.Parse_error ->
    Printf.printf "Error: Parse failed.";
    print_newline ();
    read_eval_print env
  | Lexer.Unknown ->
    Printf.printf "Error: Unknown token";
    print_newline ();
    read_eval_print env)

let initial_env =
  extend "i" (VInt 1)
	 (extend "v" (VInt 5)
		 (extend "x" (VInt 10)
			 empty_env))

let _ = read_eval_print initial_env
