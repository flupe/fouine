%{
  open Ast
  open List
%}

%token <string> IDENT

%token <string> PREFIX
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> INFIX3
%token <string> INFIX4
%token <string> INFIX5

%token <int> INT
%token LPAREN RPAREN BEGIN END SEMI
%token LET IN IF THEN ELSE DELIM FUN RARROW REC
%token MINUS MOD EQ
%token UNDERSCORE COMMA

%token TRUE FALSE
%token TRY WITH RAISE E
%token SETREF
%token DOT LARROW

%start main

%type <Ast.t list> main

%nonassoc IN
%nonassoc SEMI
%nonassoc NOELSE
%nonassoc ELSE
%right SETREF LARROW
%nonassoc below_COMMA
%left COMMA
%right RARROW
%left EQ INFIX5    (* =... <... >... |... &... $... != *)
%right INFIX4      (* @... ^... *)
(* :: *)
%left MINUS INFIX3 (* +... -... *)
%left MOD INFIX2   (* *... /... %... mod *)
%right INFIX1      (* **... *)
%nonassoc UMINUS
(* function application *)
%nonassoc INFIX0   (* #... *)
%nonassoc DOT
%nonassoc PREFIX

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

pident:
  | IDENT         { $1 }
  | operator         { $1 }

pattern_enclosed:
  | UNDERSCORE    { PAll }
  | constant      { PConst $1 }
  | pident        { PField $1 }
  | LPAREN pattern RPAREN { $2 }

operator:
  | LPAREN PREFIX RPAREN { $2 }
  | LPAREN MOD RPAREN { "mod" }
  | LPAREN EQ RPAREN { "=" }
  | LPAREN MINUS RPAREN { "-" }
  | LPAREN SETREF RPAREN { ":=" }
  | LPAREN INFIX0 RPAREN { $2 }
  | LPAREN INFIX1 RPAREN { $2 }
  | LPAREN INFIX2 RPAREN { $2 }
  | LPAREN INFIX3 RPAREN { $2 }
  | LPAREN INFIX4 RPAREN { $2 }
  | LPAREN INFIX5 RPAREN { $2 }

enclosed:
  | BEGIN expr END { $2 }
  | LPAREN expr RPAREN { $2 }
  | operator { Var $1 }
  | IDENT { Var $1 }
  | PREFIX enclosed { Call (Var $1, $2) }
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

  | LET pident pattern_list EQ expr global_lets {
      Let (PField $2, List.fold_right (fun x e -> Fun (x, e)) $3 $5) :: $6
    }

  | LET REC pident pattern_list EQ expr global_lets {
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

  | LET pident pattern_list EQ expr IN expr {
      LetIn (PField $2, List.fold_right (fun x e -> Fun (x, e)) $3 $5, $7)
    }

  | LET REC pident pattern_list EQ expr IN expr {
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

  | MINUS expr %prec UMINUS {
      match $2 with
      | Const (Int x) -> Const (Int (-x))
      | x -> Call (Var "~-", x) (* syntaxic sugar *)
    }

  | expr MOD expr   { Call (Call (Var "mod", $1), $3) }
  | expr EQ expr    { Call (Call (Var "=", $1), $3) }
  | expr MINUS expr { Call (Call (Var "-", $1), $3) }
  | expr SETREF expr { Call (Call (Var ":=", $1), $3) }

  | expr INFIX0 expr  { Call (Call (Var $2, $1), $3) }
  | expr INFIX1 expr  { Call (Call (Var $2, $1), $3) }
  | expr INFIX2 expr  { Call (Call (Var $2, $1), $3) }
  | expr INFIX3 expr  { Call (Call (Var $2, $1), $3) }
  | expr INFIX4 expr  { Call (Call (Var $2, $1), $3) }
  | expr INFIX5 expr  { Call (Call (Var $2, $1), $3) }
