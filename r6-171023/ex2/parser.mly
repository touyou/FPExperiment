%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token LET IN REC AND
%token FUN ARROW
%token PLUS TIMES MINUS DIV
%token EQ LT
%token OR
%token AND
%token IF THEN ELSE
%token LPAR RPAR
%token SEMISEMI
%token QUIT


%start toplevel
%type <Syntax.command> toplevel
%%

toplevel:
  | expr SEMISEMI { CExp $1 }
  | LET var EQ expr SEMISEMI { CDecl ($2, $4) }
  | LET REC let_and_decls SEMISEMI { CRecDecl ($3) }
  | QUIT SEMISEMI { CQuit }
;

let_and_decls:
  | var var EQ expr AND let_and_decls { ($1,$2,$4) :: $6 }
  | var var EQ expr                   { [($1,$2,$4)] }

expr:
  | LET var EQ expr IN expr     { ELet($2,$4,$6) }
  | LET REC let_and_decls IN expr { ELetRec($3,$5) }
  | FUN var ARROW expr          { EFun($2, $4) }
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | value_expr                  { $1 }
;

value_expr:
  | arith_expr                  { $1 }
  | or_bool_expr                { $1 }
;

or_bool_expr:
  | or_bool_expr OR and_bool_expr   { EOr($1,$3) }
  | and_bool_expr                   { $1 }
;

and_bool_expr:
  | and_bool_expr AND bool_expr { EAnd($1, $3) }
  | bool_expr                   { $1 }
;

bool_expr:
  | arith_expr EQ arith_expr    { EEq($1,$3) }
  | arith_expr LT arith_expr    { ELt($1,$3) }
  | app_expr                    { $1 }
;

arith_expr:
  | arith_expr PLUS factor_expr  { EAdd($1,$3) }
  | arith_expr MINUS factor_expr { ESub($1,$3) }
  | factor_expr                  { $1 }
;

factor_expr:
  | factor_expr TIMES app_expr    { EMul($1,$3) }
  | factor_expr DIV app_expr      { EDiv($1,$3) }
  | app_expr                      { $1 }
;

app_expr:
  | app_expr atomic_expr         { EApp($1, $2) }
  | atomic_expr                  { $1 }
;

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr RPAR { $2 }
;

var:
  | ID { $1 }
;
