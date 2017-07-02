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
  | MATCH
  | WITH
  | BAR
  | SEMISEMI

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 36 "parser.ml"
let yytransl_const = [|
  260 (* LET *);
  261 (* IN *);
  262 (* PLUS *);
  263 (* TIMES *);
  264 (* MINUS *);
  265 (* DIV *);
  266 (* EQ *);
  267 (* LT *);
  268 (* IF *);
  269 (* THEN *);
  270 (* ELSE *);
  271 (* LPAR *);
  272 (* RPAR *);
  273 (* FUN *);
  274 (* ARROW *);
  275 (* REC *);
  276 (* LBRACKET *);
  277 (* RBRACKET *);
  278 (* CONS *);
  279 (* COMMA *);
  280 (* MATCH *);
  281 (* WITH *);
  282 (* BAR *);
  283 (* SEMISEMI *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\005\000\005\000\007\000\007\000\
\008\000\008\000\008\000\008\000\008\000\008\000\006\000\006\000\
\004\000\004\000\004\000\009\000\009\000\009\000\010\000\010\000\
\011\000\011\000\011\000\011\000\011\000\011\000\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\007\000\006\000\008\000\006\000\004\000\003\000\
\003\000\004\000\005\000\001\000\003\000\005\000\003\000\001\000\
\001\000\001\000\001\000\005\000\002\000\003\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\001\000\002\000\001\000\
\001\000\001\000\005\000\002\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\033\000\034\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\012\000\000\000\
\000\000\032\000\039\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\000\000\000\000\000\000\000\000\
\000\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000\017\000\018\000\000\000\000\000\
\000\000\019\000\010\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\035\000\000\000\021\000\011\000\000\000\
\000\000\000\000\004\000\000\000\006\000\022\000\000\000\000\000\
\015\000\000\000\003\000\000\000\000\000\000\000\005\000\020\000\
\014\000"

let yydgoto = "\002\000\
\012\000\013\000\066\000\014\000\067\000\015\000\068\000\069\000\
\016\000\017\000\018\000"

let yysindex = "\013\000\
\041\255\000\000\000\000\000\000\000\000\009\255\077\255\077\255\
\045\255\002\255\077\255\000\000\027\255\049\255\000\000\042\255\
\073\255\000\000\000\000\045\255\056\255\043\255\057\255\250\254\
\054\255\000\000\065\255\000\000\073\255\073\255\073\255\073\255\
\073\255\073\255\073\255\000\000\045\255\077\255\045\255\072\255\
\077\255\000\000\077\255\077\255\006\255\042\255\042\255\061\255\
\061\255\007\255\000\000\073\255\073\255\090\255\014\255\045\255\
\077\255\095\255\096\255\000\000\000\000\000\000\033\000\093\255\
\033\000\000\000\000\000\098\255\099\255\077\255\077\255\000\000\
\105\255\112\255\077\255\000\000\068\255\000\000\000\000\077\255\
\033\000\020\255\000\000\077\255\000\000\000\000\033\000\092\255\
\000\000\077\255\000\000\127\255\119\255\033\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\227\255\000\000\166\255\
\097\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\189\255\212\255\243\255\
\002\000\227\255\000\000\120\255\143\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\249\255\252\255\054\000\202\255\104\000\199\255\000\000\
\066\000\064\000\242\255"

let yytablesize = 309
let yytable = "\023\000\
\024\000\021\000\036\000\027\000\025\000\077\000\061\000\062\000\
\019\000\042\000\079\000\019\000\029\000\001\000\030\000\037\000\
\043\000\040\000\071\000\016\000\063\000\016\000\026\000\089\000\
\090\000\064\000\016\000\020\000\033\000\093\000\055\000\065\000\
\054\000\058\000\056\000\059\000\060\000\036\000\036\000\097\000\
\072\000\003\000\004\000\005\000\006\000\019\000\091\000\019\000\
\034\000\074\000\035\000\073\000\007\000\028\000\029\000\008\000\
\030\000\009\000\031\000\032\000\010\000\039\000\082\000\083\000\
\011\000\038\000\029\000\085\000\030\000\041\000\033\000\044\000\
\088\000\003\000\004\000\005\000\092\000\003\000\004\000\005\000\
\022\000\057\000\095\000\086\000\048\000\049\000\050\000\008\000\
\007\000\045\000\087\000\008\000\010\000\009\000\046\000\047\000\
\010\000\052\000\053\000\070\000\011\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\075\000\030\000\030\000\076\000\
\030\000\078\000\084\000\080\000\071\000\094\000\030\000\030\000\
\081\000\030\000\030\000\030\000\028\000\028\000\028\000\028\000\
\028\000\028\000\028\000\090\000\028\000\028\000\096\000\028\000\
\051\000\000\000\000\000\000\000\000\000\028\000\028\000\000\000\
\028\000\028\000\028\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\000\000\029\000\029\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\029\000\029\000\000\000\029\000\
\029\000\029\000\027\000\027\000\000\000\027\000\000\000\027\000\
\027\000\000\000\027\000\027\000\000\000\027\000\000\000\000\000\
\000\000\000\000\000\000\027\000\027\000\000\000\027\000\027\000\
\027\000\025\000\025\000\000\000\025\000\000\000\025\000\025\000\
\000\000\025\000\025\000\000\000\025\000\000\000\000\000\000\000\
\000\000\000\000\025\000\025\000\000\000\025\000\025\000\025\000\
\026\000\026\000\000\000\026\000\000\000\026\000\026\000\000\000\
\026\000\026\000\000\000\026\000\000\000\000\000\000\000\024\000\
\000\000\026\000\026\000\000\000\026\000\026\000\026\000\024\000\
\024\000\000\000\024\000\000\000\000\000\000\000\000\000\008\000\
\000\000\024\000\000\000\024\000\024\000\024\000\000\000\008\000\
\008\000\000\000\008\000\000\000\000\000\000\000\009\000\000\000\
\000\000\008\000\000\000\008\000\008\000\008\000\009\000\009\000\
\000\000\009\000\000\000\000\000\000\000\013\000\000\000\000\000\
\009\000\000\000\009\000\009\000\009\000\013\000\013\000\000\000\
\013\000\061\000\062\000\019\000\000\000\000\000\000\000\013\000\
\000\000\013\000\000\000\013\000\000\000\000\000\000\000\063\000\
\000\000\000\000\000\000\000\000\064\000"

let yycheck = "\007\000\
\008\000\006\000\017\000\011\000\009\000\063\000\001\001\002\001\
\003\001\016\001\065\000\003\001\006\001\001\000\008\001\020\000\
\023\001\022\000\005\001\016\001\015\001\018\001\021\001\081\000\
\005\001\020\001\023\001\019\001\022\001\087\000\038\000\026\001\
\037\000\041\000\039\000\043\000\044\000\052\000\053\000\094\000\
\027\001\001\001\002\001\003\001\004\001\003\001\027\001\003\001\
\007\001\057\000\009\001\056\000\012\001\027\001\006\001\015\001\
\008\001\017\001\010\001\011\001\020\001\019\001\070\000\071\000\
\024\001\010\001\006\001\075\000\008\001\013\001\022\001\018\001\
\080\000\001\001\002\001\003\001\084\000\001\001\002\001\003\001\
\004\001\010\001\090\000\016\001\031\000\032\000\033\000\015\001\
\012\001\025\001\023\001\015\001\020\001\017\001\029\000\030\000\
\020\001\034\000\035\000\010\001\024\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\014\001\013\001\014\001\016\001\
\016\001\021\001\010\001\018\001\005\001\026\001\022\001\023\001\
\022\001\025\001\026\001\027\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\005\001\013\001\014\001\016\001\016\001\
\033\000\255\255\255\255\255\255\255\255\022\001\023\001\255\255\
\025\001\026\001\027\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\013\001\014\001\255\255\016\001\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\255\255\025\001\
\026\001\027\001\005\001\006\001\255\255\008\001\255\255\010\001\
\011\001\255\255\013\001\014\001\255\255\016\001\255\255\255\255\
\255\255\255\255\255\255\022\001\023\001\255\255\025\001\026\001\
\027\001\005\001\006\001\255\255\008\001\255\255\010\001\011\001\
\255\255\013\001\014\001\255\255\016\001\255\255\255\255\255\255\
\255\255\255\255\022\001\023\001\255\255\025\001\026\001\027\001\
\005\001\006\001\255\255\008\001\255\255\010\001\011\001\255\255\
\013\001\014\001\255\255\016\001\255\255\255\255\255\255\005\001\
\255\255\022\001\023\001\255\255\025\001\026\001\027\001\013\001\
\014\001\255\255\016\001\255\255\255\255\255\255\255\255\005\001\
\255\255\023\001\255\255\025\001\026\001\027\001\255\255\013\001\
\014\001\255\255\016\001\255\255\255\255\255\255\005\001\255\255\
\255\255\023\001\255\255\025\001\026\001\027\001\013\001\014\001\
\255\255\016\001\255\255\255\255\255\255\005\001\255\255\255\255\
\023\001\255\255\025\001\026\001\027\001\013\001\014\001\255\255\
\016\001\001\001\002\001\003\001\255\255\255\255\255\255\023\001\
\255\255\025\001\255\255\027\001\255\255\255\255\255\255\015\001\
\255\255\255\255\255\255\255\255\020\001"

let yynames_const = "\
  LET\000\
  IN\000\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIV\000\
  EQ\000\
  LT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LPAR\000\
  RPAR\000\
  FUN\000\
  ARROW\000\
  REC\000\
  LBRACKET\000\
  RBRACKET\000\
  CONS\000\
  COMMA\000\
  MATCH\000\
  WITH\000\
  BAR\000\
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
# 25 "parser.mly"
                                     ( CExp _1 )
# 260 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 26 "parser.mly"
                                     ( CDecl (_2, _4) )
# 268 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                                     ( CRecDecl (_3,_4,_6) )
# 277 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                                    ( ELet(_2,_4,_6) )
# 286 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                                    ( ELetRec(_3,_4,_6,_8) )
# 296 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                                    ( EIf(_2,_4,_6) )
# 305 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                                    ( EFun(_2,_4) )
# 313 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 35 "parser.mly"
                                    ( EEq(_1,_3) )
# 321 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 36 "parser.mly"
                                    ( ELt(_1,_3) )
# 329 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cases) in
    Obj.repr(
# 37 "parser.mly"
                                    ( EMatch(_2, _4) )
# 337 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'cases) in
    Obj.repr(
# 38 "parser.mly"
                                    ( EMatch(_2, _5) )
# 345 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list_expr) in
    Obj.repr(
# 39 "parser.mly"
                                    ( _1 )
# 352 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                                 ( [(_1, _3)] )
# 360 "parser.ml"
               : 'cases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'cases) in
    Obj.repr(
# 44 "parser.mly"
                                 ( (_1, _3) :: _5 )
# 369 "parser.ml"
               : 'cases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 48 "parser.mly"
                                     ( PCons(_1,_3) )
# 377 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_pattern) in
    Obj.repr(
# 49 "parser.mly"
                                     ( _1 )
# 384 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 53 "parser.mly"
                                     ( PInt(_1) )
# 391 "parser.ml"
               : 'atomic_pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 54 "parser.mly"
                                     ( PBool(_1) )
# 398 "parser.ml"
               : 'atomic_pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 55 "parser.mly"
                                     ( PVar(_1) )
# 405 "parser.ml"
               : 'atomic_pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 56 "parser.mly"
                                     ( PPair(_2, _4) )
# 413 "parser.ml"
               : 'atomic_pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                                     ( PNil )
# 419 "parser.ml"
               : 'atomic_pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 58 "parser.mly"
                                     ( _2 )
# 426 "parser.ml"
               : 'atomic_pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_expr) in
    Obj.repr(
# 62 "parser.mly"
                              ( ECons(_1, _3) )
# 434 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 63 "parser.mly"
                              ( _1 )
# 441 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 67 "parser.mly"
                                 ( EAdd(_1,_3) )
# 449 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 68 "parser.mly"
                                 ( ESub(_1,_3) )
# 457 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 69 "parser.mly"
                                 ( _1 )
# 464 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 73 "parser.mly"
                               ( EMul(_1,_3) )
# 472 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 74 "parser.mly"
                               ( EDiv(_1,_3) )
# 480 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 75 "parser.mly"
                               ( _1 )
# 487 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 79 "parser.mly"
                         ( EApp(_1, _2) )
# 495 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 80 "parser.mly"
                         ( _1 )
# 502 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
                      ( EConstInt(_1) )
# 509 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 84 "parser.mly"
                      ( EConstBool(_1) )
# 516 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                              ( EPair(_2, _4) )
# 524 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                      ( ENil )
# 530 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                      ( EVar(_1) )
# 537 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                      ( _2 )
# 544 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
       ( _1 )
# 551 "parser.ml"
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
