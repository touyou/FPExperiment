type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | LET
  | IN
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | EQ
  | LT
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | FUN
  | ARROW
  | REC
  | LBRACKET
  | RBRACKET
  | CONS
  | COMMA
  | SEMISEMI

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.command
