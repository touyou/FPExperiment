type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | LET
  | IN
  | REC
  | FUN
  | ARROW
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | EQ
  | LT
  | OR
  | AND
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | SEMISEMI
  | QUIT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 32 "parser.ml"
let yytransl_const = [|
  260 (* LET *);
  261 (* IN *);
  262 (* REC *);
  263 (* FUN *);
  264 (* ARROW *);
  265 (* PLUS *);
  266 (* TIMES *);
  267 (* MINUS *);
  268 (* DIV *);
  269 (* EQ *);
  270 (* LT *);
  271 (* OR *);
  272 (* AND *);
  273 (* IF *);
  274 (* THEN *);
  275 (* ELSE *);
  276 (* LPAR *);
  277 (* RPAR *);
  278 (* SEMISEMI *);
  279 (* QUIT *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\002\000\
\002\000\004\000\004\000\006\000\006\000\007\000\007\000\008\000\
\008\000\008\000\005\000\005\000\005\000\010\000\010\000\010\000\
\009\000\009\000\011\000\011\000\011\000\011\000\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\007\000\002\000\006\000\009\000\004\000\006\000\
\001\000\001\000\001\000\003\000\001\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\001\000\003\000\003\000\001\000\
\002\000\001\000\001\000\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\027\000\028\000\029\000\000\000\000\000\000\000\
\000\000\000\000\032\000\000\000\009\000\000\000\000\000\000\000\
\015\000\000\000\000\000\026\000\031\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\007\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\005\000\000\000\008\000\000\000\
\003\000\000\000\000\000\006\000"

let yydgoto = "\002\000\
\011\000\012\000\023\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000"

let yysindex = "\018\000\
\005\255\000\000\000\000\000\000\000\000\007\255\032\255\014\255\
\014\255\026\255\000\000\034\255\000\000\036\255\043\255\047\255\
\000\000\040\255\041\255\000\000\000\000\032\255\060\255\080\255\
\008\255\061\255\073\255\000\000\000\000\040\255\040\255\040\255\
\040\255\040\255\040\255\000\000\040\255\040\255\032\255\014\255\
\014\255\032\255\090\255\014\255\000\000\040\255\041\255\041\255\
\015\255\015\255\036\255\047\255\000\000\040\255\040\255\096\255\
\022\255\000\000\032\255\014\255\099\255\014\255\014\255\000\000\
\111\255\121\255\014\255\024\255\000\000\014\255\000\000\014\255\
\000\000\123\255\117\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\193\255\198\255\178\255\
\000\000\056\255\116\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\071\255\131\255\146\255\
\158\255\166\255\000\000\186\255\000\000\086\255\101\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\248\255\254\255\000\000\198\000\000\000\099\000\106\000\
\191\000\229\255\240\255"

let yytablesize = 233
let yytable = "\026\000\
\027\000\036\000\047\000\048\000\024\000\003\000\004\000\005\000\
\006\000\021\000\021\000\007\000\022\000\042\000\003\000\004\000\
\005\000\025\000\001\000\039\000\007\000\008\000\043\000\030\000\
\009\000\031\000\063\000\010\000\072\000\036\000\008\000\057\000\
\058\000\009\000\021\000\061\000\056\000\036\000\036\000\059\000\
\003\000\004\000\005\000\064\000\030\000\073\000\031\000\028\000\
\032\000\033\000\037\000\066\000\038\000\068\000\069\000\029\000\
\065\000\034\000\071\000\009\000\018\000\074\000\035\000\075\000\
\024\000\024\000\024\000\024\000\024\000\024\000\018\000\018\000\
\040\000\018\000\018\000\024\000\018\000\018\000\044\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\041\000\
\024\000\024\000\022\000\024\000\024\000\045\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\060\000\022\000\
\022\000\023\000\022\000\022\000\062\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\067\000\023\000\023\000\
\021\000\023\000\023\000\070\000\021\000\063\000\021\000\072\000\
\021\000\021\000\021\000\021\000\052\000\021\000\021\000\019\000\
\021\000\021\000\076\000\019\000\053\000\019\000\000\000\019\000\
\019\000\019\000\019\000\000\000\019\000\019\000\020\000\019\000\
\019\000\000\000\020\000\000\000\020\000\000\000\020\000\020\000\
\020\000\020\000\016\000\020\000\020\000\000\000\020\000\020\000\
\000\000\000\000\017\000\000\000\016\000\016\000\000\000\016\000\
\016\000\000\000\016\000\016\000\017\000\017\000\013\000\017\000\
\017\000\000\000\017\000\017\000\000\000\000\000\012\000\000\000\
\013\000\000\000\000\000\013\000\013\000\010\000\013\000\013\000\
\012\000\000\000\011\000\012\000\012\000\000\000\012\000\012\000\
\000\000\000\000\010\000\010\000\000\000\010\000\010\000\011\000\
\011\000\000\000\011\000\011\000\046\000\046\000\046\000\046\000\
\000\000\000\000\000\000\054\000\055\000\049\000\050\000\051\000\
\051\000"

let yycheck = "\008\000\
\009\000\018\000\030\000\031\000\007\000\001\001\002\001\003\001\
\004\001\003\001\003\001\007\001\006\001\006\001\001\001\002\001\
\003\001\004\001\001\000\022\000\007\001\017\001\025\000\009\001\
\020\001\011\001\005\001\023\001\005\001\046\000\017\001\040\000\
\041\000\020\001\003\001\044\000\039\000\054\000\055\000\042\000\
\001\001\002\001\003\001\022\001\009\001\022\001\011\001\022\001\
\013\001\014\001\010\001\060\000\012\001\062\000\063\000\022\001\
\059\000\015\001\067\000\020\001\005\001\070\000\016\001\072\000\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\013\001\018\001\019\001\005\001\021\001\022\001\018\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\008\001\
\018\001\019\001\005\001\021\001\022\001\021\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\013\001\018\001\
\019\001\005\001\021\001\022\001\013\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\019\001\018\001\019\001\
\005\001\021\001\022\001\013\001\009\001\005\001\011\001\005\001\
\013\001\014\001\015\001\016\001\034\000\018\001\019\001\005\001\
\021\001\022\001\022\001\009\001\035\000\011\001\255\255\013\001\
\014\001\015\001\016\001\255\255\018\001\019\001\005\001\021\001\
\022\001\255\255\009\001\255\255\011\001\255\255\013\001\014\001\
\015\001\016\001\005\001\018\001\019\001\255\255\021\001\022\001\
\255\255\255\255\005\001\255\255\015\001\016\001\255\255\018\001\
\019\001\255\255\021\001\022\001\015\001\016\001\005\001\018\001\
\019\001\255\255\021\001\022\001\255\255\255\255\005\001\255\255\
\015\001\255\255\255\255\018\001\019\001\005\001\021\001\022\001\
\015\001\255\255\005\001\018\001\019\001\255\255\021\001\022\001\
\255\255\255\255\018\001\019\001\255\255\021\001\022\001\018\001\
\019\001\255\255\021\001\022\001\030\000\031\000\032\000\033\000\
\255\255\255\255\255\255\037\000\038\000\032\000\033\000\034\000\
\035\000"

let yynames_const = "\
  LET\000\
  IN\000\
  REC\000\
  FUN\000\
  ARROW\000\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIV\000\
  EQ\000\
  LT\000\
  OR\000\
  AND\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LPAR\000\
  RPAR\000\
  SEMISEMI\000\
  QUIT\000\
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
# 26 "parser.mly"
                  ( CExp _1 )
# 219 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                             ( CDecl (_2, _4) )
# 227 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 28 "parser.mly"
                                     ( CRecDecl (_3,_4,_6) )
# 236 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
                  ( CQuit )
# 242 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                                ( ELet(_2,_4,_6) )
# 251 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                                             ( ELetRec(_3,_4,_6,_8) )
# 261 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                                ( EFun(_2, _4) )
# 269 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                                ( EIf(_2,_4,_6) )
# 278 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value_expr) in
    Obj.repr(
# 37 "parser.mly"
                                ( _1 )
# 285 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 41 "parser.mly"
                                ( _1 )
# 292 "parser.ml"
               : 'value_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_bool_expr) in
    Obj.repr(
# 42 "parser.mly"
                                ( _1 )
# 299 "parser.ml"
               : 'value_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_bool_expr) in
    Obj.repr(
# 46 "parser.mly"
                                    ( EOr(_1,_3) )
# 307 "parser.ml"
               : 'or_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_bool_expr) in
    Obj.repr(
# 47 "parser.mly"
                                    ( _1 )
# 314 "parser.ml"
               : 'or_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 51 "parser.mly"
                                ( EAnd(_1, _3) )
# 322 "parser.ml"
               : 'and_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 52 "parser.mly"
                                ( _1 )
# 329 "parser.ml"
               : 'and_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 56 "parser.mly"
                                ( EEq(_1,_3) )
# 337 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 57 "parser.mly"
                                ( ELt(_1,_3) )
# 345 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 58 "parser.mly"
                                ( _1 )
# 352 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 62 "parser.mly"
                                 ( EAdd(_1,_3) )
# 360 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 63 "parser.mly"
                                 ( ESub(_1,_3) )
# 368 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 64 "parser.mly"
                                 ( _1 )
# 375 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 68 "parser.mly"
                                  ( EMul(_1,_3) )
# 383 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 69 "parser.mly"
                                  ( EDiv(_1,_3) )
# 391 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 70 "parser.mly"
                                  ( _1 )
# 398 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 74 "parser.mly"
                                 ( EApp(_1, _2) )
# 406 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 75 "parser.mly"
                                 ( _1 )
# 413 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 79 "parser.mly"
                   ( EConstInt(_1) )
# 420 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 80 "parser.mly"
                   ( EConstBool(_1) )
# 427 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
                   ( EVar(_1) )
# 434 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                   ( _2 )
# 441 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
       ( _1 )
# 448 "parser.ml"
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
