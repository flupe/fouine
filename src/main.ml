open Expr

let _ =
  try
    Lexing.from_channel stdin
    |> Parser.main Lexer.token
    |> ignore
  with _ ->
    print_endline "SYNTAX ERROR FDP"

