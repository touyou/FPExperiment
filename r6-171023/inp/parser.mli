type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | LET
  | IN
  | REC
  | AND
  | FUN
  | ARROW
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | EQ
  | LT
  | OR
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | SEMISEMI
  | QUIT

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.command
