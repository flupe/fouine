%{
  open Ast
  open List

  let mk_infix x op y = Call (Call (Var op, x), y)
  let mk_prefix op x = Call (Var op, x)

  exception UnknownTypeHint
  exception SyntaxError
%}

%token <string> IDENT
%token <string> STRING
%token <char> CHAR
%token <string> CONSTRUCTOR

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
%token SQUOTE BAR TYPE OF STAR EOF

%token TRUE FALSE
%token TRY WITH RAISE
%token SETREF CONS
%token DOT LARROW COLON MATCH

%start main

%type <Ast.prog> main

(* %nonassoc IN *)
(* %nonassoc IDENT *)
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
%left MOD INFIX2 STAR   (* *... /... %... mod *)
%right INFIX1      (* **... *)
%nonassoc UMINUS
(* function application *)
%nonassoc INFIX0   (* #... *)
%nonassoc DOT
%nonassoc PREFIX


%%

integer: INT { Int $1 }

boolean:
  | TRUE { Bool true }
  | FALSE { Bool false }

comma_separated_type_spec:
  | type_spec { [$1] }
  | comma_separated_type_spec COMMA type_spec { $3 :: $1 }

star_separated_type_spec:
  | enclosed_type_spec { [$1] }
  | star_separated_type_spec STAR enclosed_type_spec { $3 :: $1 }

type_params: 
  | { [] }
  | enclosed_type_spec { [$1] }
  | LPAREN comma_separated_type_spec RPAREN { $2 }

enclosed_type_spec:
  | LPAREN type_spec RPAREN { $2 }
  | SQUOTE IDENT { SUnbound $2 }
  | type_params IDENT { SSubtype ($2, $1) }

type_spec:
  | star_separated_type_spec STAR enclosed_type_spec { STuple (List.rev ($3 :: $1)) }
  | enclosed_type_spec RARROW type_spec { SArrow ($1, $3) }
  | enclosed_type_spec { $1 }

unit:
  | LPAREN RPAREN { Unit }
  | BEGIN END { Unit }

constant:
  | boolean { $1 }
  | integer { $1 }
  | unit    { $1 }
  | STRING  { String $1 }
  | CHAR  { Char $1 }

array_access:
  | enclosed DOT LPAREN seq_expr RPAREN { $1, $4 }

pattern:
  | l = separated_nonempty_list(COMMA, pattern_enclosed) {
      match l with
      | [x] -> x
      | _ -> PTuple l
    }

  | constructor pattern_enclosed {
      PConstructor ($1,
        match $2 with
        | PTuple l -> l
        | x -> [x]
      )
  }


pattern_list:
  | l = nonempty_list(pattern_enclosed) { l }

pattern_enclosed:
  | UNDERSCORE    { PAll }
  | constructor   { PConstructor ($1, []) }
  | constant      { PConst $1 }
  | ident        { PField $1 }
  | LPAREN pattern RPAREN { $2 }
  | LPAREN pattern COLON type_spec RPAREN { PConstraint ($2, $4) }
  | pattern_enclosed CONS pattern_enclosed { PConstructor ("(::)", [$1; $3]) }

operator:
  | PREFIX { $1 }
  | MOD { "mod" }
  | EQ { "=" }
  | MINUS { "-" }
  | SETREF { ":=" }
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
  | LPAREN seq_expr COLON type_spec RPAREN { Constraint ($2, $4) }
  | LBRACKET semi_expr_list RBRACKET {
      List.fold_left (fun e x -> Constructor ("(::)", [x; e])) (Constructor ("[]", [])) $2
    }
  | LBRACKET BAR semi_expr_list BAR RBRACKET { Array (List.rev $3) }
  | ident { Var $1 }
  | PREFIX enclosed { mk_prefix $1 $2 }
  | constant { Const $1 }
  | array_access {
      let arr, e = $1 in
      ArrayRead (arr, e)
    }
  | constructor { Constructor ($1, []) }

main:
  | statement DELIM { $1 }
  | statement EOF { $1 }

statement:
  | global_lets { $1 }
  | type_decl { [$1] }
  | seq_expr { [ Expr $1 ] }

comma_separated_type_params_desc:
  | SQUOTE IDENT { [$2] }
  | comma_separated_type_params_desc COMMA SQUOTE IDENT { $4 :: $1 }

type_params_desc:
  | { [] }
  | SQUOTE IDENT { [$2] }
  | LPAREN comma_separated_type_params_desc RPAREN { List.rev $2 }

constructor:
  | CONSTRUCTOR { $1 }
  | LBRACKET RBRACKET { "[]" }
  | LPAREN CONS RPAREN { "(::)" }

constr_def:
  | constructor { ($1, []) }
  | constructor OF star_separated_type_spec { ($1, List.rev $3) }

constr_list:
  | constr_def { [$1] }
  | BAR constr_def { [$2] }
  | constr_list BAR constr_def { $3 :: $1 }

type_def:
  | type_spec { Alias $1 }
  | constr_list { Sum (List.rev $1) }

type_decl:
  | TYPE type_params_desc IDENT EQ type_def { TypeDef ($2, $3, $5) }

global_lets:
  | { [] }

  | LET pattern EQ seq_expr global_lets { Decl ($2, $4) :: $5 }

  | LET ident pattern_list EQ seq_expr global_lets {
      Decl (PField $2, List.fold_right (fun x e -> Fun (x, e)) $3 $5) :: $6
    }

  | LET REC ident EQ seq_expr global_lets {
      DeclRec ($3, $5) :: $6
  } 

  | LET REC ident pattern_list EQ seq_expr global_lets {
      DeclRec ($3, List.fold_right (fun x e -> Fun (x, e)) $4 $6) :: $7
  } 

comma_list:
  | comma_list COMMA expr { $3 :: $1 }
  | expr COMMA expr { [$3; $1] }

pattern_matching:
  | pattern RARROW seq_expr { [($1, $3)] }
  | BAR pattern RARROW seq_expr { [($2, $4)] }
  | pattern_matching BAR pattern RARROW seq_expr { ($3, $5) :: $1 }

expr:
  | comma_list %prec below_COMMA { Tuple (List.rev $1) }

  | args = enclosed+ {
      List.fold_left (fun e a ->
        match e with
        | Constructor (name, []) -> Constructor (name,
            match a with
            | Tuple l -> l
            | x -> [x]
          )
        | Constructor _ -> raise SyntaxError
        | _ -> Call(e, a)
      ) (hd args) (tl args)
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
  | TRY seq_expr WITH pattern RARROW seq_expr { TryWith ($2, $4, $6) }
  | RAISE enclosed { Raise $2 }

  | MATCH expr WITH pattern_matching { MatchWith ($2, List.rev $4) }

  | array_access LARROW expr {
      let arr, key = $1 in
      ArraySet (arr, key, $3)
    }

  | MINUS expr %prec UMINUS {
      match $2 with
      | Const (Int x) -> Const (Int (-x))
      | x -> mk_prefix "~-" x
    }

  | expr CONS expr   { Constructor ("(::)", [$1; $3]) }
  | expr STAR expr   { mk_infix $1 "*" $3 }
  | expr MOD expr    { mk_infix $1 "mod" $3 }
  | expr EQ expr     { mk_infix $1 "=" $3 }
  | expr MINUS expr  { mk_infix $1 "-" $3 }
  | expr SETREF expr { mk_infix $1 ":=" $3 }

  | expr INFIX0 expr  { mk_infix $1 $2 $3 }
  | expr INFIX1 expr  { mk_infix $1 $2 $3 }
  | expr INFIX2 expr  { mk_infix $1 $2 $3 }
  | expr INFIX3 expr  { mk_infix $1 $2 $3 }
  | expr INFIX4 expr  { mk_infix $1 $2 $3 }
  | expr INFIX5 expr  { mk_infix $1 $2 $3 }

