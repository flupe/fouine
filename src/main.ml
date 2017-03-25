open Expr
open Print

let _ =
  let prog = 
    try
      Lexing.from_channel stdin
      |> Parser.main Lexer.token
    with _ ->
       failwith @@ "syntax error"
  in
  print prog;
  try
    Interpreter.eval prog
    |> print_constant
  with Interpreter.InterpretationError ->
    print_endline "error while interpreting the program"

