%{
  open Expr
%}

%token <string> IDENT
%token <int> INT
%token TRUE FALSE
%token LPAREN RPAREN
%token LET IN IF THEN ELSE DELIM FUN ARROW PRINT EOL REC
%token PLUS MINUS MULT OR AND LT GT LEQ GEQ EQ NOT NEQ

%start main

%type <Expr.expr> main
%type <Expr.expr> expr
%type <string list> list_of_idents

%left ELSE IN
%left PRINT
%left PLUS MINUS OR
%left MULT AND

%nonassoc NOT LET IF THEN DELIM FUN ARROW INT
%nonassoc TRUE FALSE LPAREN RPAREN LT GT LEQ GEQ EQ NEQ IDENT

%%

main:
  | expr DELIM { $1 }

list_of_idents:
  | IDENT { [$1] }
  | list_of_idents IDENT { $2 :: $1 }

expr:
  | LPAREN expr RPAREN { $2 }

  | TRUE  { Constant (Bool true) }
  | FALSE { Constant (Bool false) }
  | INT   { Constant (Int $1) }

  | LET IDENT EQ expr IN expr { Let($2, $4, $6) }
  | LET IDENT IDENT EQ expr IN expr { Let($2, Fun($3, $5), $7) }

  | LET IDENT list_of_idents EQ expr IN expr {
      Let ($2, List.fold_left (fun e x -> Fun(x, e)) $5 $3, $7)
    }

  | FUN IDENT ARROW expr { Fun($2, $4) }

  | IF expr THEN expr           { IfThenElse($2, $4, Constant Unit) }
  | IF expr THEN expr ELSE expr { IfThenElse($2, $4, $6) }

  | PRINT expr { Print $2 }

  | IDENT { Var $1 }

  | NOT expr        { UnaryOp(Not, $2) }
  | expr PLUS expr  { BinaryOp(Plus, $1, $3) }
  | expr MINUS expr { BinaryOp(Minus, $1, $3) }
  | expr MULT expr  { BinaryOp(Mult, $1, $3) }
  | expr OR expr    { BinaryOp(Or, $1, $3) }
  | expr AND expr   { BinaryOp(And, $1, $3) }
  | expr LT expr    { BinaryOp(Lt, $1, $3) }
  | expr GT expr    { BinaryOp(Gt, $1, $3) }
  | expr LEQ expr   { BinaryOp(Leq, $1, $3) }
  | expr GEQ expr   { BinaryOp(Geq, $1, $3) }
  | expr EQ expr    { BinaryOp(Eq, $1, $3) }
  | expr NEQ expr   { BinaryOp(Neq, $1, $3) }


