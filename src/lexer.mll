{
  open Parser
  exception Eof
}

rule token = parse
  | "true" { TRUE }
  | "false" { FALSE }
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
  | "&&" { OR }

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

  | [' ' '\t'] { token lexbuf }
  | ['0'-'9']+ as s { INT(int_of_string s) }
  | ['_' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { IDENT(i) }

  | eof  { raise Eof }
