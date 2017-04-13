{
  open Parser
  exception Eof
}

let blank = [' ' '\r' '\t' '\n']
let digit = ['0'-'9']

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
  | ":=" { SETREF }
  | "rec" { REC }
  | "mod" { MOD }
  | "->" { RARROW }
  | "<-" { LARROW }
  | "&&" { AND }
  | "||" { OR }
  | '_' { UNDERSCORE }

  | "<>" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '!' { BANG }

  | '=' { EQ }
  | '<' { LT }
  | '>' { GT }

  | ';' { SEMI }
  | '.' { DOT }
  | ',' { COMMA }
  | ";;" { DELIM }

  | digit+ as s { INT(int_of_string s) }
  | ['_' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { IDENT(i) }
  | blank { token lexbuf }

  | eof  { raise Eof }
