%{
  open Types
%}

%token <float> FLOAT
%token TRUE FALSE
%token <Types.exprS list> LIST
%token LET
%token VARIABLE
%token <Types.exprS list> TUPLE
%token COMMENT
%token TUPLE
%token DBLSEMI
%token IF THEN ELSE
%token OR
%token AND
%token NOT
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token <string> COMPOP
%token EQ
%token NEQ
%token VARIABLE
%token LET

%nonassoc FLOAT
%nonassoc ELSE
%nonassoc OR AND
%nonassoc EQ NEQ
%nonassoc NOT
%nonassoc COMPOP
%nonassoc VARIABLE
%nonassoc LET

%left PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Types.exprS> main
%%

main:
  | headEx DBLSEMI               { $1 }
;

headEx:
  | expr                         { $1 }
;

expr:
  | FLOAT                        { NumS $1 }
  | FALSE                        { BoolS false }
  | TRUE                         { BoolS true }
  | LIST                         { ListS $1 }
  | TUPLE                        { TupleS $1 }
  | LET expr expr                { LetS $2 $3}
  | VARIABLE expr                { VarS $2}
  | COMMENT                      { CommentS }
  | IF expr THEN expr ELSE expr  { IfS ($2, $4, $6) }
  | expr OR expr                 { OrS ($1, $3) }
  | expr AND expr                { AndS ($1, $3) }
  | NOT expr                     { NotS $2 }
  | expr PLUS expr 				       { ArithS ("+", $1, $3) }
  | expr MINUS expr 			       { ArithS ("-", $1, $3) }
  | expr TIMES expr 			       { ArithS ("*", $1, $3) }
  | expr DIVIDE expr 			       { ArithS ("/", $1, $3) }
  | expr COMPOP expr 			       { CompS ($2, $1, $3) }
  | expr EQ expr 				         { EqS ($1, $3) }
  | expr NEQ expr 				       { NeqS ($1, $3) }
;

