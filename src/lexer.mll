{
  open Parser
  exception Eof
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | ['0'-'9']+ as s { INT(int_of_string s) }
  | ['_' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9' '\'']* as i { IDENT(i) }
  | "true" { TRUE }
  | "false" { FALSE }

  (* this should be automated *)
  | "let" { LET }
  | "fun" { FUN }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "not" { NOT }
  | "->" { ARROW }
  | "print" { PRINT }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }

  | "&&" { AND }
  | "||" { OR }

  | "<>" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | '=' { EQ }
  | '<' { LT }
  | '>' { GT }

  | ";;" { DELIM }

  | eof  { raise Eof }
