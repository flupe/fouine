%{
  open Ast
  open List
%}

%token <string> IDENT
%token <int> INT
%token LPAREN RPAREN BEGIN END SEMI
%token LET IN IF THEN ELSE DELIM FUN RARROW REC
%token PLUS MINUS MULT DIV MOD OR AND LT GT LEQ GEQ EQ NEQ
%token UNDERSCORE COMMA

%token TRUE FALSE
%token TRY WITH RAISE E
%token SETREF BANG
%token DOT LARROW

%start main

%type <Ast.t list> main

%nonassoc IN
%nonassoc SEMI
%nonassoc NOELSE
%nonassoc ELSE
%nonassoc LARROW
%right SETREF
%nonassoc below_COMMA
%left COMMA
%right RARROW
%right OR
%right AND
%left PLUS MINUS
%left MOD
%left MULT DIV
%nonassoc LT GT LEQ GEQ EQ NEQ
%nonassoc UMINUS
%nonassoc DOT
%nonassoc BANG

%%

boolean:
  | TRUE { Bool true }
  | FALSE { Bool false }

integer:
  | INT { Int $1 }

unit:
  | LPAREN RPAREN { Unit }
  | BEGIN END { Unit }

constant:
  | boolean { $1 }
  | integer { $1 }
  | unit    { $1 }

array_access:
  | enclosed DOT LPAREN expr RPAREN { $1, $4 }

pattern:
  | l = separated_nonempty_list(COMMA, pattern_enclosed) {
      match l with
      | [x] -> x
      | _ -> PTuple l
    }

pattern_list:
  | l = nonempty_list(pattern_enclosed) { l }

pattern_enclosed:
  | UNDERSCORE    { PAll }
  | constant      { PConst $1 }
  | IDENT         { PField $1 }
  | LPAREN pattern RPAREN { $2 }

enclosed:
  | BEGIN expr END { $2 }
  | LPAREN expr RPAREN { $2 }
  | BANG enclosed { Deref ($2) }
  | IDENT { Var $1 }
  | constant { Const $1 }
  | array_access {
      let arr, e = $1 in
      ArrayRead (arr, e)
    }

/****************************************************/
/****************************************************/

main:
  | global DELIM { $1 }

global:
  | global_lets { $1 }
  | expr { [ $1 ] }

global_lets:
  | { [] }

  | LET pattern EQ expr global_lets { Let ($2, $4) :: $5 }

  | LET IDENT pattern_list EQ expr global_lets {
      Let (PField $2, List.fold_right (fun x e -> Fun (x, e)) $3 $5) :: $6
    }

  | LET REC IDENT pattern_list EQ expr global_lets {
      LetRec ($3, List.fold_right (fun x e -> Fun (x, e)) $4 $6) :: $7
    }

comma_list:
  | comma_list COMMA expr { $3 :: $1 }
  | expr COMMA expr { [$3; $1] }

expr:
  | comma_list %prec below_COMMA { Tuple (List.rev $1) }
  | expr SEMI expr  { Seq ($1, $3) }

  /* function calls */
  | args = enclosed+ {
      List.fold_left (fun e a -> Call(e, a)) (hd args) (tl args)
    }

  | LET pattern EQ expr IN expr { LetIn ($2, $4, $6) }

  | LET IDENT pattern_list EQ expr IN expr {
      LetIn (PField $2, List.fold_right (fun x e -> Fun (x, e)) $3 $5, $7)
    }

  | LET REC IDENT pattern_list EQ expr IN expr {
      LetRecIn ($3, List.fold_right (fun x e -> Fun (x, e)) $4 $6, $8)
    }

  | FUN pattern_list RARROW expr {
      List.fold_right (fun x e -> Fun (x, e)) $2 $4
    }

  | IF expr THEN expr ELSE expr { IfThenElse ($2, $4, $6) }
  | IF expr THEN expr %prec NOELSE { IfThenElse ($2, $4, Const Unit) }
  | TRY expr WITH E pattern RARROW expr { TryWith ($2, $5, $7) }
  | RAISE enclosed { Raise $2 }

  | array_access LARROW expr {
      let arr, key = $1 in
      ArraySet (arr, key, $3)
    }

  | expr SETREF expr   { BinaryOp (SetRef, $1, $3) }
  | MINUS expr %prec UMINUS { UnaryOp (UMinus, $2) }

  | expr PLUS expr  { BinaryOp (Plus, $1, $3) }
  | expr MINUS expr { BinaryOp (Minus, $1, $3) }
  | expr MULT expr  { BinaryOp (Mult, $1, $3) }
  | expr DIV expr   { BinaryOp (Div, $1, $3) }
  | expr MOD expr   { BinaryOp (Mod, $1, $3) }
  | expr OR expr    { BinaryOp (Or, $1, $3) }
  | expr AND expr   { BinaryOp (And, $1, $3) }
  | expr LT expr    { BinaryOp (Lt, $1, $3) }
  | expr GT expr    { BinaryOp (Gt, $1, $3) }
  | expr LEQ expr   { BinaryOp (Leq, $1, $3) }
  | expr GEQ expr   { BinaryOp (Geq, $1, $3) }
  | expr EQ expr    { BinaryOp (Eq, $1, $3) }
  | expr NEQ expr   { BinaryOp (Neq, $1, $3) }
