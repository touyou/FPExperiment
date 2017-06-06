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
  263 (* AND *);
  264 (* FUN *);
  265 (* ARROW *);
  266 (* PLUS *);
  267 (* TIMES *);
  268 (* MINUS *);
  269 (* DIV *);
  270 (* EQ *);
  271 (* LT *);
  272 (* OR *);
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
\001\000\001\000\001\000\001\000\004\000\004\000\002\000\002\000\
\002\000\002\000\002\000\005\000\005\000\007\000\007\000\008\000\
\008\000\009\000\009\000\009\000\006\000\006\000\006\000\011\000\
\011\000\011\000\010\000\010\000\012\000\012\000\012\000\012\000\
\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\004\000\002\000\006\000\004\000\006\000\005\000\
\004\000\006\000\001\000\001\000\001\000\003\000\001\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\001\000\003\000\
\003\000\001\000\002\000\001\000\001\000\001\000\001\000\003\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\029\000\030\000\031\000\000\000\000\000\000\000\
\000\000\000\000\034\000\000\000\011\000\000\000\000\000\000\000\
\017\000\000\000\000\000\028\000\033\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\
\000\000\000\000\003\000\000\000\009\000\000\000\000\000\000\000\
\000\000\008\000\000\000\002\000\000\000\000\000\000\000\007\000\
\010\000\000\000\005\000"

let yydgoto = "\002\000\
\011\000\012\000\039\000\040\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000"

let yysindex = "\012\000\
\007\255\000\000\000\000\000\000\000\000\070\255\017\255\015\255\
\015\255\255\254\000\000\003\255\000\000\039\255\010\255\037\255\
\000\000\040\255\001\255\000\000\000\000\017\255\034\255\043\255\
\077\255\047\255\061\255\000\000\000\000\040\255\040\255\040\255\
\040\255\040\255\040\255\000\000\040\255\040\255\017\255\023\255\
\015\255\015\255\017\255\080\255\015\255\000\000\040\255\001\255\
\001\255\046\255\046\255\039\255\037\255\000\000\040\255\040\255\
\084\255\015\255\000\000\024\255\000\000\086\255\015\255\081\255\
\015\255\000\000\015\255\000\000\096\255\015\255\102\255\000\000\
\000\000\017\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\207\255\212\255\219\255\
\000\000\056\255\128\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\074\255\146\255\
\164\255\182\255\189\255\000\000\227\255\000\000\092\255\110\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\042\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\248\255\253\255\220\255\000\000\158\000\000\000\078\000\
\081\000\220\000\231\255\240\255"

let yytablesize = 258
let yytable = "\026\000\
\027\000\036\000\023\000\024\000\048\000\049\000\062\000\003\000\
\004\000\005\000\006\000\037\000\001\000\038\000\007\000\003\000\
\004\000\005\000\025\000\021\000\028\000\044\000\007\000\008\000\
\029\000\034\000\009\000\058\000\067\000\010\000\036\000\008\000\
\060\000\061\000\009\000\057\000\064\000\075\000\036\000\036\000\
\003\000\004\000\005\000\035\000\059\000\068\000\006\000\041\000\
\030\000\066\000\031\000\042\000\032\000\033\000\069\000\030\000\
\071\000\031\000\072\000\009\000\020\000\073\000\020\000\006\000\
\045\000\026\000\026\000\026\000\026\000\026\000\026\000\020\000\
\021\000\020\000\020\000\022\000\020\000\020\000\026\000\021\000\
\026\000\046\000\043\000\026\000\026\000\026\000\026\000\026\000\
\026\000\026\000\058\000\026\000\026\000\063\000\026\000\026\000\
\024\000\065\000\024\000\070\000\067\000\024\000\024\000\024\000\
\024\000\024\000\024\000\024\000\074\000\024\000\024\000\053\000\
\024\000\024\000\025\000\054\000\025\000\000\000\000\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\000\000\025\000\
\025\000\000\000\025\000\025\000\023\000\000\000\023\000\000\000\
\000\000\023\000\000\000\023\000\000\000\023\000\023\000\023\000\
\000\000\023\000\023\000\000\000\023\000\023\000\021\000\000\000\
\021\000\000\000\000\000\021\000\000\000\021\000\000\000\021\000\
\021\000\021\000\000\000\021\000\021\000\000\000\021\000\021\000\
\022\000\000\000\022\000\000\000\000\000\022\000\000\000\022\000\
\000\000\022\000\022\000\022\000\000\000\022\000\022\000\000\000\
\022\000\022\000\018\000\000\000\018\000\050\000\051\000\052\000\
\052\000\019\000\000\000\019\000\000\000\018\000\000\000\018\000\
\018\000\000\000\018\000\018\000\019\000\000\000\019\000\019\000\
\000\000\019\000\019\000\012\000\000\000\012\000\000\000\000\000\
\013\000\000\000\013\000\000\000\000\000\000\000\000\000\015\000\
\012\000\012\000\000\000\012\000\012\000\013\000\013\000\014\000\
\013\000\013\000\015\000\000\000\015\000\015\000\000\000\015\000\
\015\000\000\000\014\000\000\000\014\000\014\000\000\000\014\000\
\014\000\047\000\047\000\047\000\047\000\000\000\000\000\000\000\
\055\000\056\000"

let yycheck = "\008\000\
\009\000\018\000\006\000\007\000\030\000\031\000\043\000\001\001\
\002\001\003\001\004\001\011\001\001\000\013\001\008\001\001\001\
\002\001\003\001\004\001\003\001\022\001\025\000\008\001\017\001\
\022\001\016\001\020\001\005\001\005\001\023\001\047\000\017\001\
\041\000\042\000\020\001\039\000\045\000\074\000\055\000\056\000\
\001\001\002\001\003\001\007\001\022\001\022\001\005\001\014\001\
\010\001\058\000\012\001\009\001\014\001\015\001\063\000\010\001\
\065\000\012\001\067\000\020\001\005\001\070\000\007\001\022\001\
\018\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\003\001\018\001\019\001\006\001\021\001\022\001\005\001\003\001\
\007\001\021\001\006\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\005\001\018\001\019\001\014\001\021\001\022\001\
\005\001\014\001\007\001\019\001\005\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\007\001\018\001\019\001\034\000\
\021\001\022\001\005\001\035\000\007\001\255\255\255\255\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\019\001\255\255\021\001\022\001\005\001\255\255\007\001\255\255\
\255\255\010\001\255\255\012\001\255\255\014\001\015\001\016\001\
\255\255\018\001\019\001\255\255\021\001\022\001\005\001\255\255\
\007\001\255\255\255\255\010\001\255\255\012\001\255\255\014\001\
\015\001\016\001\255\255\018\001\019\001\255\255\021\001\022\001\
\005\001\255\255\007\001\255\255\255\255\010\001\255\255\012\001\
\255\255\014\001\015\001\016\001\255\255\018\001\019\001\255\255\
\021\001\022\001\005\001\255\255\007\001\032\000\033\000\034\000\
\035\000\005\001\255\255\007\001\255\255\016\001\255\255\018\001\
\019\001\255\255\021\001\022\001\016\001\255\255\018\001\019\001\
\255\255\021\001\022\001\005\001\255\255\007\001\255\255\255\255\
\005\001\255\255\007\001\255\255\255\255\255\255\255\255\005\001\
\018\001\019\001\255\255\021\001\022\001\018\001\019\001\005\001\
\021\001\022\001\016\001\255\255\018\001\019\001\255\255\021\001\
\022\001\255\255\016\001\255\255\018\001\019\001\255\255\021\001\
\022\001\030\000\031\000\032\000\033\000\255\255\255\255\255\255\
\037\000\038\000"

let yynames_const = "\
  LET\000\
  IN\000\
  REC\000\
  AND\000\
  FUN\000\
  ARROW\000\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIV\000\
  EQ\000\
  LT\000\
  OR\000\
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
# 227 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                             ( CDecl (_2, _4) )
# 235 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'let_and_decls) in
    Obj.repr(
# 28 "parser.mly"
                                   ( CRecDecl (_3) )
# 242 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
                  ( CQuit )
# 248 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'let_and_decls) in
    Obj.repr(
# 33 "parser.mly"
                                      ( (_1,_2,_4) :: _6 )
# 258 "parser.ml"
               : 'let_and_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                                      ( [(_1,_2,_4)] )
# 267 "parser.ml"
               : 'let_and_decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                                ( ELet(_2,_4,_6) )
# 276 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'let_and_decls) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                                  ( ELetRec(_3,_5) )
# 284 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                                ( EFun(_2, _4) )
# 292 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                                ( EIf(_2,_4,_6) )
# 301 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value_expr) in
    Obj.repr(
# 41 "parser.mly"
                                ( _1 )
# 308 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 45 "parser.mly"
                                ( _1 )
# 315 "parser.ml"
               : 'value_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_bool_expr) in
    Obj.repr(
# 46 "parser.mly"
                                ( _1 )
# 322 "parser.ml"
               : 'value_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_bool_expr) in
    Obj.repr(
# 50 "parser.mly"
                                    ( EOr(_1,_3) )
# 330 "parser.ml"
               : 'or_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_bool_expr) in
    Obj.repr(
# 51 "parser.mly"
                                    ( _1 )
# 337 "parser.ml"
               : 'or_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 55 "parser.mly"
                                ( EAnd(_1, _3) )
# 345 "parser.ml"
               : 'and_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 56 "parser.mly"
                                ( _1 )
# 352 "parser.ml"
               : 'and_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 60 "parser.mly"
                                ( EEq(_1,_3) )
# 360 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 61 "parser.mly"
                                ( ELt(_1,_3) )
# 368 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 62 "parser.mly"
                                ( _1 )
# 375 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 66 "parser.mly"
                                 ( EAdd(_1,_3) )
# 383 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 67 "parser.mly"
                                 ( ESub(_1,_3) )
# 391 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 68 "parser.mly"
                                 ( _1 )
# 398 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 72 "parser.mly"
                                  ( EMul(_1,_3) )
# 406 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 73 "parser.mly"
                                  ( EDiv(_1,_3) )
# 414 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 74 "parser.mly"
                                  ( _1 )
# 421 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 78 "parser.mly"
                                 ( EApp(_1, _2) )
# 429 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 79 "parser.mly"
                                 ( _1 )
# 436 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
                   ( EConstInt(_1) )
# 443 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 84 "parser.mly"
                   ( EConstBool(_1) )
# 450 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                   ( EVar(_1) )
# 457 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                   ( _2 )
# 464 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
       ( _1 )
# 471 "parser.ml"
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
