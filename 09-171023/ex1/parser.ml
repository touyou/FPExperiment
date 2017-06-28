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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 33 "parser.ml"
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
  280 (* SEMISEMI *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\005\000\005\000\004\000\004\000\004\000\006\000\
\006\000\006\000\007\000\007\000\008\000\008\000\008\000\008\000\
\008\000\008\000\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\007\000\006\000\008\000\006\000\004\000\003\000\
\003\000\001\000\003\000\001\000\003\000\003\000\001\000\003\000\
\003\000\001\000\002\000\001\000\001\000\001\000\005\000\002\000\
\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\021\000\022\000\025\000\000\000\000\000\000\000\
\000\000\000\000\028\000\000\000\000\000\010\000\000\000\000\000\
\020\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\026\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\000\000\000\000\002\000\000\000\000\000\000\000\023\000\
\000\000\004\000\000\000\006\000\000\000\003\000\000\000\005\000"

let yydgoto = "\002\000\
\011\000\012\000\020\000\013\000\014\000\015\000\016\000\017\000"

let yysindex = "\001\000\
\006\255\000\000\000\000\000\000\000\000\025\255\010\255\010\255\
\013\255\021\255\000\000\022\255\197\255\000\000\050\255\038\255\
\000\000\000\000\013\255\040\255\042\255\041\255\032\255\052\255\
\000\000\000\000\038\255\038\255\038\255\038\255\038\255\038\255\
\038\255\000\000\013\255\010\255\013\255\063\255\010\255\000\000\
\010\255\010\255\050\255\050\255\087\255\087\255\254\254\000\000\
\038\255\038\255\080\255\014\255\013\255\010\255\084\255\083\255\
\000\000\010\255\010\255\000\000\100\255\108\255\010\255\000\000\
\019\255\000\000\010\255\000\000\010\255\000\000\110\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\162\255\000\000\118\255\058\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\138\255\158\255\174\255\178\255\162\255\000\000\
\078\255\098\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\249\255\252\255\046\000\085\000\051\000\064\000\243\255"

let yytablesize = 219
let yytable = "\022\000\
\023\000\001\000\034\000\027\000\024\000\028\000\003\000\004\000\
\005\000\006\000\003\000\004\000\005\000\021\000\035\000\018\000\
\038\000\007\000\059\000\031\000\008\000\007\000\009\000\069\000\
\008\000\010\000\009\000\018\000\052\000\010\000\051\000\055\000\
\053\000\056\000\057\000\034\000\034\000\060\000\003\000\004\000\
\005\000\025\000\070\000\019\000\018\000\026\000\062\000\040\000\
\061\000\036\000\065\000\066\000\008\000\039\000\041\000\068\000\
\032\000\010\000\033\000\071\000\037\000\072\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\042\000\018\000\018\000\
\054\000\018\000\045\000\046\000\047\000\043\000\044\000\018\000\
\018\000\018\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\058\000\016\000\016\000\027\000\016\000\028\000\049\000\
\050\000\063\000\064\000\016\000\016\000\016\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\067\000\017\000\017\000\
\059\000\017\000\069\000\048\000\000\000\000\000\000\000\017\000\
\017\000\017\000\015\000\015\000\000\000\015\000\000\000\015\000\
\015\000\000\000\015\000\015\000\000\000\015\000\000\000\000\000\
\000\000\000\000\000\000\015\000\015\000\015\000\013\000\013\000\
\000\000\013\000\000\000\013\000\013\000\000\000\013\000\013\000\
\000\000\013\000\000\000\000\000\000\000\000\000\000\000\013\000\
\013\000\013\000\014\000\014\000\000\000\014\000\012\000\014\000\
\014\000\000\000\014\000\014\000\000\000\014\000\012\000\012\000\
\000\000\012\000\008\000\014\000\014\000\014\000\009\000\000\000\
\012\000\012\000\008\000\008\000\000\000\008\000\009\000\009\000\
\000\000\009\000\000\000\000\000\008\000\008\000\000\000\000\000\
\009\000\009\000\027\000\000\000\028\000\000\000\029\000\030\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\000"

let yycheck = "\007\000\
\008\000\001\000\016\000\006\001\009\000\008\001\001\001\002\001\
\003\001\004\001\001\001\002\001\003\001\004\001\019\000\003\001\
\021\000\012\001\005\001\022\001\015\001\012\001\017\001\005\001\
\015\001\020\001\017\001\003\001\036\000\020\001\035\000\039\000\
\037\000\041\000\042\000\049\000\050\000\024\001\001\001\002\001\
\003\001\021\001\024\001\019\001\003\001\024\001\054\000\016\001\
\053\000\010\001\058\000\059\000\015\001\013\001\023\001\063\000\
\007\001\020\001\009\001\067\000\019\001\069\000\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\018\001\013\001\014\001\
\010\001\016\001\029\000\030\000\031\000\027\000\028\000\022\001\
\023\001\024\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\010\001\013\001\014\001\006\001\016\001\008\001\032\000\
\033\000\014\001\016\001\022\001\023\001\024\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\010\001\013\001\014\001\
\005\001\016\001\005\001\031\000\255\255\255\255\255\255\022\001\
\023\001\024\001\005\001\006\001\255\255\008\001\255\255\010\001\
\011\001\255\255\013\001\014\001\255\255\016\001\255\255\255\255\
\255\255\255\255\255\255\022\001\023\001\024\001\005\001\006\001\
\255\255\008\001\255\255\010\001\011\001\255\255\013\001\014\001\
\255\255\016\001\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\024\001\005\001\006\001\255\255\008\001\005\001\010\001\
\011\001\255\255\013\001\014\001\255\255\016\001\013\001\014\001\
\255\255\016\001\005\001\022\001\023\001\024\001\005\001\255\255\
\023\001\024\001\013\001\014\001\255\255\016\001\013\001\014\001\
\255\255\016\001\255\255\255\255\023\001\024\001\255\255\255\255\
\023\001\024\001\006\001\255\255\008\001\255\255\010\001\011\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\022\001"

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
# 24 "parser.mly"
                                     ( CExp _1 )
# 213 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                                     ( CDecl (_2, _4) )
# 221 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 26 "parser.mly"
                                     ( CRecDecl (_3,_4,_6) )
# 230 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
                                    ( ELet(_2,_4,_6) )
# 239 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                                    ( ELetRec(_3,_4,_6,_8) )
# 249 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                                    ( EIf(_2,_4,_6) )
# 258 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                                    ( EFun(_2,_4) )
# 266 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 34 "parser.mly"
                                    ( EEq(_1,_3) )
# 274 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 35 "parser.mly"
                                    ( ELt(_1,_3) )
# 282 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list_expr) in
    Obj.repr(
# 36 "parser.mly"
                                    ( _1 )
# 289 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_expr) in
    Obj.repr(
# 40 "parser.mly"
                              ( ECons(_1, _3) )
# 297 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 41 "parser.mly"
                              ( _1 )
# 304 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 45 "parser.mly"
                                 ( EAdd(_1,_3) )
# 312 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 46 "parser.mly"
                                 ( ESub(_1,_3) )
# 320 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 47 "parser.mly"
                                 ( _1 )
# 327 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 51 "parser.mly"
                               ( EMul(_1,_3) )
# 335 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 52 "parser.mly"
                               ( EDiv(_1,_3) )
# 343 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 53 "parser.mly"
                               ( _1 )
# 350 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 57 "parser.mly"
                         ( EApp(_1, _2) )
# 358 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 58 "parser.mly"
                         ( _1 )
# 365 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 61 "parser.mly"
                      ( EConstInt(_1) )
# 372 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 62 "parser.mly"
                      ( EConstBool(_1) )
# 379 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                              ( EPair(_2, _4) )
# 387 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                      ( ENil )
# 393 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                      ( EVar(_1) )
# 400 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                      ( _2 )
# 407 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
       ( _1 )
# 414 "parser.ml"
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
