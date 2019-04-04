%{
    open AST
%}

%token <int> INT
%token <string> VAR
%token <string> QUOTED_STRING
%token TRUE FALSE
%token PLUS TIMES CONCAT BAR
%token LPAREN RPAREN LBRACE RBRACE
%token IF THEN ELSE
%token LET BE IN
%token LAMBDA SUCHTHAT
%token STRING_TYPE NUMBER_TYPE BOOLEAN_TYPE ARROW
%token CONS EMPTY_LIST CASE SEMI COMMA
%token EOF

%start <AST.expression> expr

%%

syntax_type : STRING_TYPE { String }
     | NUMBER_TYPE { Number }
     | BOOLEAN_TYPE { Boolean }
     | LPAREN t1=syntax_type ARROW t2=syntax_type RPAREN { Function (t1, t2) }

expr: v=VAR { Var v }
    | n=INT { Val (Num n) }
    | s=QUOTED_STRING { Val (Str s) }
    | TRUE { Val (Bool true) }
    | FALSE { Val (Bool false) }
    | LPAREN e1=expr PLUS e2=expr RPAREN { Plus (e1, e2) }
    | LPAREN e1=expr TIMES e2=expr RPAREN { Times (e1, e2) }
    | LPAREN e1=expr CONCAT e2=expr RPAREN { Cat (e1, e2) }
    | EMPTY_LIST { Empty }
    | LPAREN e=expr CONS es=expr { Cons (e, es) }
    | BAR e=expr BAR { Length e }
    | IF e=expr THEN e1=expr ELSE e2=expr { Ite (e, e1, e2) }
    | LET v=VAR BE e1=expr IN e2=expr { Let (v, e1, e2) }
    | LAMBDA LBRACE t=syntax_type RBRACE LPAREN x=VAR RPAREN SUCHTHAT e=expr { Lam (x, t, e) }
    | LPAREN e1=expr RPAREN LPAREN e2=expr RPAREN { Ap (e1, e2) }
    | CASE LBRACE e0=expr SEMI LPAREN x=VAR COMMA y=VAR RPAREN ARROW e1=expr RBRACE LPAREN e=expr RPAREN
          { Case (e0, x, y, e1, e) }
    | EOF { failwith "Unexpected end of file!" }
    ;
