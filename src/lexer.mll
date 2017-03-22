{
  open Parser
  exception Eof
}

rule token = parse
  | ['0'-'9']+ as s { INT(int_of_string s) }
  | ['a'-'z']['A'-'Z''a'-'z''0'-'9']* as i { IDENT(i) }
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

  | '=' { EQ }
  | "<>" { NEQ }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LEQ }
  | ">=" { GEQ }

  | ";;" { DELIM }

  | eof  { raise Eof }
