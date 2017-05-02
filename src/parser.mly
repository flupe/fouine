%{
  open Ast
  open List

  let mk_infix x op y = Call (Call (Var op, x), y)
  let mk_prefix op x = Call (Var op, x)
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
%token LPAREN RPAREN LBRACKET RBRACKET BEGIN END SEMI
%token LET IN IF THEN ELSE DELIM FUN RARROW REC
%token MINUS MOD EQ
%token UNDERSCORE COMMA

%token TRUE FALSE
%token TRY WITH RAISE E
%token SETREF CONS
%token DOT LARROW

%start main

%type <Ast.prog> main

(* %nonassoc IN *)
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc NOELSE
%nonassoc ELSE
%right SETREF LARROW
%nonassoc below_COMMA
%left COMMA
(* %right RARROW *)
%left EQ INFIX5    (* =... <... >... |... &... $... != *)
%right INFIX4      (* @... ^... *)
%right CONS
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
  | enclosed DOT LPAREN seq_expr RPAREN { $1, $4 }

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
  | ident        { PField $1 }
  | LPAREN pattern RPAREN { $2 }

operator:
  | PREFIX { $1 }
  | MOD { "mod" }
  | EQ { "=" }
  | MINUS { "-" }
  | SETREF { ":=" }
  | CONS { "::" }
  | INFIX0 { $1 }
  | INFIX1 { $1 }
  | INFIX2 { $1 }
  | INFIX3 { $1 }
  | INFIX4 { $1 }
  | INFIX5 { $1 }

ident:
  | IDENT { $1 }
  | LPAREN operator RPAREN { $2 }

seq_expr:
  | expr %prec below_SEMI { $1 }
  (* | expr SEMI { Seq ($1, Const Unit) } *)
  | expr SEMI seq_expr { Seq ($1, $3) }

semi_expr_list:
  | expr { [$1] }
  | semi_expr_list SEMI expr { $3 :: $1 }

enclosed:
  | BEGIN seq_expr END { $2 }
  | LPAREN seq_expr RPAREN { $2 }
  | LBRACKET RBRACKET { Empty }
  | LBRACKET semi_expr_list RBRACKET {
      List.fold_left (fun e x -> mk_infix x "::" e) Empty $2
    }
  | ident { Var $1 }
  | PREFIX enclosed { mk_prefix $1 $2 }
  | constant { Const $1 }
  | array_access {
      let arr, e = $1 in
      ArrayRead (arr, e)
    }

main:
  | statement DELIM { $1 }

statement:
  | global_lets { $1 }
  | seq_expr { [ Expr $1 ] }

global_lets:
  | { [] }

  | LET pattern EQ seq_expr global_lets { Decl ($2, $4) :: $5 }

  | LET ident pattern_list EQ seq_expr global_lets {
      Decl (PField $2, List.fold_right (fun x e -> Fun (x, e)) $3 $5) :: $6
    }

  | LET REC ident pattern_list EQ seq_expr global_lets {
      DeclRec ($3, List.fold_right (fun x e -> Fun (x, e)) $4 $6) :: $7
  } 

comma_list:
  | comma_list COMMA expr { $3 :: $1 }
  | expr COMMA expr { [$3; $1] }

expr:
  | comma_list %prec below_COMMA { Tuple (List.rev $1) }

  /* function calls */
  | args = enclosed+ {
      List.fold_left (fun e a -> Call(e, a)) (hd args) (tl args)
    }

  | LET pattern EQ seq_expr IN seq_expr { Let ($2, $4, $6) }

  | LET ident pattern_list EQ seq_expr IN seq_expr {
      Let (PField $2, List.fold_right (fun x e -> Fun (x, e)) $3 $5, $7)
    }

  | LET REC ident pattern_list EQ seq_expr IN seq_expr {
      LetRec ($3, List.fold_right (fun x e -> Fun (x, e)) $4 $6, $8)
    }

  | FUN pattern_list RARROW seq_expr {
      List.fold_right (fun x e -> Fun (x, e)) $2 $4
    }

  | IF expr THEN expr ELSE expr { IfThenElse ($2, $4, $6) }
  | IF expr THEN expr %prec NOELSE { IfThenElse ($2, $4, Const Unit) }
  | TRY seq_expr WITH E pattern RARROW seq_expr { TryWith ($2, $5, $7) }
  | RAISE enclosed { Raise $2 }

  | array_access LARROW expr {
      let arr, key = $1 in
      ArraySet (arr, key, $3)
    }

  | MINUS expr %prec UMINUS {
      match $2 with
      | Const (Int x) -> Const (Int (-x))
      | x -> mk_prefix "~-" x
    }

  | expr MOD expr    { mk_infix $1 "mod" $3 }
  | expr EQ expr     { mk_infix $1 "=" $3 }
  | expr MINUS expr  { mk_infix $1 "-" $3 }
  | expr SETREF expr { mk_infix $1 ":=" $3 }
  | expr CONS expr   { mk_infix $1 "::" $3 }

  | expr INFIX0 expr  { mk_infix $1 $2 $3 }
  | expr INFIX1 expr  { mk_infix $1 $2 $3 }
  | expr INFIX2 expr  { mk_infix $1 $2 $3 }
  | expr INFIX3 expr  { mk_infix $1 $2 $3 }
  | expr INFIX4 expr  { mk_infix $1 $2 $3 }
  | expr INFIX5 expr  { mk_infix $1 $2 $3 }

