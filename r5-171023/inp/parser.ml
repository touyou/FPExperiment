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
  | AND
  | OR
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | SEMISEMI

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 28 "parser.ml"
let yytransl_const = [|
  260 (* LET *);
  261 (* IN *);
  262 (* PLUS *);
  263 (* TIMES *);
  264 (* MINUS *);
  265 (* DIV *);
  266 (* EQ *);
  267 (* LT *);
  268 (* AND *);
  269 (* OR *);
  270 (* IF *);
  271 (* THEN *);
  272 (* ELSE *);
  273 (* LPAR *);
  274 (* RPAR *);
  275 (* SEMISEMI *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\004\000\004\000\004\000\005\000\005\000\005\000\006\000\
\006\000\006\000\006\000\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\006\000\003\000\003\000\003\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\001\000\001\000\
\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\016\000\017\000\018\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\015\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\014\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\009\000\010\000\015\000\011\000\012\000\013\000"

let yysindex = "\013\000\
\001\255\000\000\000\000\000\000\000\000\017\255\005\255\005\255\
\000\000\125\255\027\255\141\255\000\000\000\000\019\255\017\255\
\134\255\254\254\005\255\005\255\000\000\023\255\023\255\023\255\
\023\255\023\255\023\255\005\255\022\255\005\255\000\000\139\255\
\139\255\141\255\141\255\037\255\037\255\000\000\000\000\120\255\
\005\255\129\255\005\255\000\000\123\255\005\255\139\255\139\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\075\255\036\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\255\
\099\255\051\255\066\255\084\255\093\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\105\255\111\255"

let yygindex = "\000\000\
\000\000\249\255\034\000\129\000\133\000\131\000"

let yytablesize = 158
let yytable = "\017\000\
\018\000\003\000\004\000\005\000\006\000\003\000\004\000\005\000\
\016\000\019\000\020\000\032\000\033\000\001\000\007\000\031\000\
\005\000\008\000\007\000\014\000\040\000\008\000\042\000\003\000\
\004\000\005\000\005\000\005\000\028\000\005\000\005\000\041\000\
\022\000\045\000\023\000\047\000\024\000\025\000\048\000\008\000\
\012\000\012\000\022\000\012\000\023\000\012\000\012\000\012\000\
\012\000\029\000\012\000\012\000\000\000\012\000\012\000\010\000\
\010\000\000\000\010\000\000\000\010\000\010\000\010\000\010\000\
\000\000\010\000\010\000\000\000\010\000\010\000\011\000\011\000\
\000\000\011\000\000\000\011\000\011\000\011\000\011\000\009\000\
\011\000\011\000\000\000\011\000\011\000\000\000\009\000\009\000\
\007\000\009\000\009\000\000\000\009\000\009\000\000\000\007\000\
\007\000\008\000\007\000\007\000\000\000\007\000\007\000\006\000\
\008\000\008\000\000\000\008\000\008\000\003\000\008\000\008\000\
\000\000\006\000\006\000\004\000\006\000\006\000\000\000\003\000\
\003\000\000\000\003\000\003\000\043\000\004\000\004\000\043\000\
\004\000\004\000\000\000\019\000\020\000\000\000\019\000\020\000\
\019\000\020\000\044\000\000\000\019\000\020\000\000\000\021\000\
\046\000\019\000\020\000\026\000\030\000\027\000\019\000\020\000\
\036\000\037\000\034\000\035\000\038\000\039\000"

let yycheck = "\007\000\
\008\000\001\001\002\001\003\001\004\001\001\001\002\001\003\001\
\004\001\012\001\013\001\019\000\020\000\001\000\014\001\018\001\
\005\001\017\001\014\001\003\001\028\000\017\001\030\000\001\001\
\002\001\003\001\015\001\016\001\010\001\018\001\019\001\010\001\
\006\001\041\000\008\001\043\000\010\001\011\001\046\000\017\001\
\005\001\006\001\006\001\008\001\008\001\010\001\011\001\012\001\
\013\001\016\000\015\001\016\001\255\255\018\001\019\001\005\001\
\006\001\255\255\008\001\255\255\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\255\255\018\001\019\001\005\001\006\001\
\255\255\008\001\255\255\010\001\011\001\012\001\013\001\005\001\
\015\001\016\001\255\255\018\001\019\001\255\255\012\001\013\001\
\005\001\015\001\016\001\255\255\018\001\019\001\255\255\012\001\
\013\001\005\001\015\001\016\001\255\255\018\001\019\001\005\001\
\012\001\013\001\255\255\015\001\016\001\005\001\018\001\019\001\
\255\255\015\001\016\001\005\001\018\001\019\001\255\255\015\001\
\016\001\255\255\018\001\019\001\005\001\015\001\016\001\005\001\
\018\001\019\001\255\255\012\001\013\001\255\255\012\001\013\001\
\012\001\013\001\019\001\255\255\012\001\013\001\255\255\019\001\
\016\001\012\001\013\001\007\001\015\001\009\001\012\001\013\001\
\024\000\025\000\022\000\023\000\026\000\027\000"

let yynames_const = "\
  LET\000\
  IN\000\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIV\000\
  EQ\000\
  LT\000\
  AND\000\
  OR\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LPAR\000\
  RPAR\000\
  SEMISEMI\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 22 "parser.mly"
                  ( CExp _1 )
# 171 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 23 "parser.mly"
                             ( CDecl (_2, _4) )
# 179 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                                ( ELet(_2,_4,_6) )
# 188 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 28 "parser.mly"
                                ( EIf(_2,_4,_6) )
# 197 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
                                ( EAnd(_1,_3) )
# 205 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                                ( EOr(_1,_3) )
# 213 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 32 "parser.mly"
                                ( EEq(_1,_3) )
# 221 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 33 "parser.mly"
                                ( ELt(_1,_3) )
# 229 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 34 "parser.mly"
                                ( _1 )
# 236 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 38 "parser.mly"
                                 ( EAdd(_1,_3) )
# 244 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 39 "parser.mly"
                                 ( ESub(_1,_3) )
# 252 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 40 "parser.mly"
                                 ( _1 )
# 259 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 44 "parser.mly"
                                  ( EMul(_1,_3) )
# 267 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 45 "parser.mly"
                                  ( EDiv(_1,_3) )
# 275 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 46 "parser.mly"
                                  ( _1 )
# 282 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 50 "parser.mly"
                   ( EConstInt(_1) )
# 289 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 51 "parser.mly"
                   ( EConstBool(_1) )
# 296 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                   ( EVar(_1) )
# 303 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                   ( _2 )
# 310 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
       ( _1 )
# 317 "parser.ml"
               : 'var))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.command)
