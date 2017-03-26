open Ast
open Print

let _ =
  let prog =
    try
      Lexing.from_channel stdin
      |> Parser.main Lexer.token
    with _ ->
      print_endline "syntax error";
      exit 1;
  in
  print prog;
  try
    Interpreter.eval prog
    |> Interpreter.print_result
  with Interpreter.InterpretationError ->
    print_endline "error while interpreting the program"

