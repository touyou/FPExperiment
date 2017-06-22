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
  | ANDAND
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
  272 (* ANDAND *);
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
\002\000\004\000\004\000\005\000\005\000\006\000\006\000\006\000\
\007\000\007\000\007\000\008\000\008\000\008\000\009\000\009\000\
\010\000\010\000\010\000\010\000\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\007\000\002\000\006\000\008\000\006\000\004\000\
\001\000\003\000\001\000\003\000\001\000\003\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\001\000\002\000\001\000\
\001\000\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\025\000\026\000\027\000\000\000\000\000\000\000\
\000\000\000\000\030\000\000\000\000\000\000\000\013\000\000\000\
\000\000\000\000\024\000\029\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\005\000\000\000\007\000\000\000\003\000\000\000\006\000"

let yydgoto = "\002\000\
\011\000\012\000\022\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000"

let yysindex = "\001\000\
\006\255\000\000\000\000\000\000\000\000\000\255\031\255\013\255\
\013\255\018\255\000\000\025\255\035\255\041\255\000\000\200\255\
\036\255\040\255\000\000\000\000\031\255\057\255\077\255\015\255\
\058\255\070\255\000\000\000\000\040\255\040\255\040\255\040\255\
\040\255\040\255\040\255\040\255\000\000\031\255\013\255\013\255\
\031\255\087\255\013\255\000\000\041\255\000\000\036\255\036\255\
\044\255\044\255\040\255\040\255\093\255\022\255\000\000\031\255\
\013\255\089\255\013\255\013\255\000\000\097\255\110\255\013\255\
\023\255\000\000\013\255\000\000\013\255\000\000\116\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\186\255\168\255\000\000\140\255\
\098\255\053\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\179\255\000\000\113\255\128\255\
\148\255\160\255\068\255\083\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\248\255\254\255\000\000\094\000\095\000\234\255\249\255\
\116\000\242\255"

let yytablesize = 214
let yytable = "\025\000\
\026\000\001\000\020\000\037\000\023\000\021\000\003\000\004\000\
\005\000\006\000\049\000\050\000\007\000\003\000\004\000\005\000\
\024\000\020\000\038\000\007\000\041\000\042\000\008\000\047\000\
\048\000\009\000\060\000\069\000\010\000\008\000\054\000\055\000\
\009\000\020\000\058\000\053\000\037\000\037\000\056\000\027\000\
\003\000\004\000\005\000\061\000\070\000\035\000\028\000\036\000\
\063\000\029\000\065\000\066\000\031\000\062\000\032\000\068\000\
\030\000\022\000\071\000\009\000\072\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\039\000\022\000\022\000\
\020\000\022\000\022\000\043\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\040\000\020\000\020\000\021\000\
\020\000\020\000\044\000\021\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\057\000\021\000\021\000\019\000\021\000\
\021\000\059\000\019\000\064\000\019\000\067\000\019\000\019\000\
\019\000\019\000\060\000\019\000\019\000\017\000\019\000\019\000\
\069\000\017\000\045\000\017\000\046\000\017\000\017\000\017\000\
\017\000\000\000\017\000\017\000\018\000\017\000\017\000\000\000\
\018\000\000\000\018\000\000\000\018\000\018\000\018\000\018\000\
\016\000\018\000\018\000\000\000\018\000\018\000\051\000\052\000\
\014\000\000\000\016\000\016\000\000\000\016\000\016\000\000\000\
\016\000\016\000\014\000\014\000\015\000\014\000\014\000\000\000\
\014\000\014\000\000\000\000\000\011\000\000\000\015\000\015\000\
\000\000\015\000\015\000\000\000\015\000\015\000\011\000\010\000\
\000\000\011\000\011\000\000\000\011\000\011\000\009\000\000\000\
\000\000\010\000\000\000\000\000\010\000\010\000\000\000\010\000\
\010\000\000\000\000\000\009\000\009\000\000\000\009\000\009\000\
\031\000\000\000\032\000\000\000\033\000\034\000"

let yycheck = "\008\000\
\009\000\001\000\003\001\018\000\007\000\006\001\001\001\002\001\
\003\001\004\001\033\000\034\000\007\001\001\001\002\001\003\001\
\004\001\003\001\021\000\007\001\006\001\024\000\017\001\031\000\
\032\000\020\001\005\001\005\001\023\001\017\001\039\000\040\000\
\020\001\003\001\043\000\038\000\051\000\052\000\041\000\022\001\
\001\001\002\001\003\001\022\001\022\001\010\001\022\001\012\001\
\057\000\015\001\059\000\060\000\009\001\056\000\011\001\064\000\
\016\001\005\001\067\000\020\001\069\000\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\013\001\018\001\019\001\
\005\001\021\001\022\001\018\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\008\001\018\001\019\001\005\001\
\021\001\022\001\021\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\013\001\018\001\019\001\005\001\021\001\
\022\001\013\001\009\001\019\001\011\001\013\001\013\001\014\001\
\015\001\016\001\005\001\018\001\019\001\005\001\021\001\022\001\
\005\001\009\001\029\000\011\001\030\000\013\001\014\001\015\001\
\016\001\255\255\018\001\019\001\005\001\021\001\022\001\255\255\
\009\001\255\255\011\001\255\255\013\001\014\001\015\001\016\001\
\005\001\018\001\019\001\255\255\021\001\022\001\035\000\036\000\
\005\001\255\255\015\001\016\001\255\255\018\001\019\001\255\255\
\021\001\022\001\015\001\016\001\005\001\018\001\019\001\255\255\
\021\001\022\001\255\255\255\255\005\001\255\255\015\001\016\001\
\255\255\018\001\019\001\255\255\021\001\022\001\015\001\005\001\
\255\255\018\001\019\001\255\255\021\001\022\001\005\001\255\255\
\255\255\015\001\255\255\255\255\018\001\019\001\255\255\021\001\
\022\001\255\255\255\255\018\001\019\001\255\255\021\001\022\001\
\009\001\255\255\011\001\255\255\013\001\014\001"

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
  ANDAND\000\
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
# 210 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                                     ( CDecl (_2, _4) )
# 218 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 28 "parser.mly"
                                     ( CRecDecl (_3,_4,_6) )
# 227 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
                                     ( CQuit )
# 233 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                                    ( ELet(_2,_4,_6) )
# 242 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                                    ( ELetRec(_3,_4,_6,_8) )
# 252 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                                    ( EIf(_2,_4,_6) )
# 261 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                                    ( EFun(_2,_4) )
# 269 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_expr) in
    Obj.repr(
# 37 "parser.mly"
                                    ( _1 )
# 276 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 40 "parser.mly"
                                     ( EOr(_1, _3) )
# 284 "parser.ml"
               : 'or_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 41 "parser.mly"
                                     ( _1 )
# 291 "parser.ml"
               : 'or_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'comp_expr) in
    Obj.repr(
# 45 "parser.mly"
                                      ( EAnd(_1, _3) )
# 299 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comp_expr) in
    Obj.repr(
# 46 "parser.mly"
                                      ( _1 )
# 306 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 50 "parser.mly"
                                    ( EEq(_1,_3) )
# 314 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 51 "parser.mly"
                                    ( ELt(_1,_3) )
# 322 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 52 "parser.mly"
                                    ( _1 )
# 329 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 56 "parser.mly"
                                 ( EAdd(_1,_3) )
# 337 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 57 "parser.mly"
                                 ( ESub(_1,_3) )
# 345 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 58 "parser.mly"
                                 ( _1 )
# 352 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 62 "parser.mly"
                               ( EMul(_1,_3) )
# 360 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 63 "parser.mly"
                               ( EDiv(_1,_3) )
# 368 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 64 "parser.mly"
                               ( _1 )
# 375 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 68 "parser.mly"
                         ( EApp(_1, _2) )
# 383 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 69 "parser.mly"
                         ( _1 )
# 390 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 72 "parser.mly"
                   ( EConstInt(_1) )
# 397 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 73 "parser.mly"
                   ( EConstBool(_1) )
# 404 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
                   ( EVar(_1) )
# 411 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                   ( _2 )
# 418 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
       ( _1 )
# 425 "parser.ml"
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
