{
  open Parser
  exception Eof
}

let blank = [' ' '\r' '\t']
let digit = ['0'-'9']

rule token = parse
  | "let" { LET }
  | "fun" { FUN }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "rec" { REC }
  | "not" { NOT }
  | "->" { ARROW }
  | "print" { PRINT }
  | "&&" { AND }
  | "||" { OR }

  | "<>" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }

  | '=' { EQ }
  | '<' { LT }
  | '>' { GT }

  | ";;" { DELIM }

  | digit+ as s { INT(int_of_string s) }
  | ['_' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { IDENT(i) }
  | blank { token lexbuf }

  | eof  { raise Eof }
