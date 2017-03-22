open Expr

let _ =
  try
    Lexing.from_channel stdin
    |> Parser.main Lexer.token
    |> Expr.print
  with _ ->
    print_endline "SYNTAX ERROR FDP"

