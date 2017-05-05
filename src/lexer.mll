{
  open Parser
  open Lexing
  exception Eof
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
  | "raise" { RAISE }
  | 'E' { E }
  | "rec" { REC }
  | "mod" { MOD }
  | "->" { RARROW }
  | "<-" { LARROW }
  | ":=" { SETREF }
  | "::" { CONS }
  | '_' { UNDERSCORE }
  | '\'' { SQUOTE }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '-' { MINUS }

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
  | ['@' '-'] operator_char * { INFIX4 (lexeme lexbuf) }
  | ['=' '<' '>' '|' '&' '$'] operator_char * { INFIX5 (lexeme lexbuf) }

  | digit+ as s { INT (int_of_string s) }
  | ['_' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { IDENT (i) }
  | blank { token lexbuf }

  | eof  { raise Eof }
