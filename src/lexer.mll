{
  open Parser
  open Lexing
  exception Eof

  let in_string = ref false
}

let blank = [' ' '\r' '\t' '\n']
let digit = ['0'-'9']

let operator_char = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']

rule token = parse
  | "begin" { BEGIN }
  | "end" { END }
  | "let" { LET }
  | "fun" { FUN }
  | "in" { IN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "try" { TRY }
  | "with" { WITH }
  | "match" { MATCH }
  | "raise" { RAISE }
  | "rec" { REC }
  | "mod" { MOD }
  | "type" { TYPE }
  | "of" { OF }
  | "->" { RARROW }
  | "<-" { LARROW }
  | ":=" { SETREF }
  | "::" { CONS }
  | '_' { UNDERSCORE }
  | '\'' { SQUOTE }

  | '(' { LPAREN }
  | '|' { BAR }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | "[|" { ALBRACKET }
  | "|]" { ARBRACKET }
  | '-' { MINUS }
  | '*' { STAR }

  | '=' { EQ }

  | ';' { SEMI }
  | ':' { COLON }
  | '.' { DOT }
  | ',' { COMMA }
  | ";;" { DELIM }

  (* prefix operators *)
  | ('!' | ['?' '~'] operator_char) operator_char * { PREFIX (lexeme lexbuf) }
  (* infix operators *)
  | '#' operator_char + { INFIX0 (lexeme lexbuf) }
  | "**" operator_char * { INFIX1 (lexeme lexbuf) }
  | ['*' '/' '%'] operator_char * { INFIX2 (lexeme lexbuf) }
  | ['+' '-'] operator_char * { INFIX3 (lexeme lexbuf) }
  | ['@' '^'] operator_char * { INFIX4 (lexeme lexbuf) }
  | ['=' '<' '>' '|' '&' '$'] operator_char * { INFIX5 (lexeme lexbuf) }

  | '"' ([^'"']* as s) '"' { STRING (s) }
  | '\'' ([^'\''] as c) '\'' { CHAR (c) }
  | digit+ as s { INT (int_of_string s) }
  | ['_' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { IDENT (i) }
  | ['A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { CONSTRUCTOR (i) }
  | blank { token lexbuf }

  | eof  { EOF }
