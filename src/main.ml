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
    let error x =
      print_endline (red "Uncaught exception E :");
      Interpreter.print_result x
    in
    let success = Interpreter.print_result in
    Interpreter.eval success error prog
  with Interpreter.InterpretationError ->
    print_endline "error while interpreting the program"

