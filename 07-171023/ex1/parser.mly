%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token LET IN REC
%token FUN ARROW
%token PLUS TIMES MINUS DIV
%token EQ LT
%token OR
%token ANDAND
%token IF THEN ELSE
%token LPAR RPAR
%token SEMISEMI
%token QUIT


%start toplevel
%type <Syntax.command> toplevel
%%

toplevel:
  | expr SEMISEMI                    { CExp $1 }
  | LET var EQ expr SEMISEMI         { CDecl ($2, $4) }
  | LET REC var var EQ expr SEMISEMI { CRecDecl ($3,$4,$6) }
;

expr:
  | LET var EQ expr IN expr         { ELet($2,$4,$6) }
  | LET REC var var EQ expr IN expr { ELetRec($3,$4,$6,$8) }
  | IF expr THEN expr ELSE expr     { EIf($2,$4,$6) }
  | FUN var ARROW expr              { EFun($2,$4) }
  | or_expr                         { $1 }

or_expr:
  | or_expr OR and_expr              { EOr($1, $3) }
  | and_expr                         { $1 }
;

and_expr:
  | and_expr ANDAND comp_expr         { EAnd($1, $3) }
  | comp_expr                         { $1 }
;

comp_expr:
  | arith_expr EQ arith_expr        { EEq($1,$3) }
  | arith_expr LT arith_expr        { ELt($1,$3) }
  | arith_expr                      { $1 }
;

arith_expr:
  | arith_expr PLUS factor_expr  { EAdd($1,$3) }
  | arith_expr MINUS factor_expr { ESub($1,$3) }
  | factor_expr                  { $1 }
;

factor_expr:
  | factor_expr TIMES app_expr { EMul($1,$3) }
  | factor_expr DIV app_expr   { EDiv($1,$3) }
  | app_expr                   { $1 }
;

app_expr:
  | app_expr atomic_expr { EApp($1, $2) }
  | atomic_expr          { $1 }

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr RPAR { $2 }
;

var:
  | ID { $1 }
;
