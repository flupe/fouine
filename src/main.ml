open Ast
open Print

let _ =
  let env = ref Interpreter.Env.empty in
  while true do
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
      let error _ x =
        print_endline (red "Uncaught exception E :");
        Interpreter.print_result x
      in
      let success e x =
        env := e;
        Interpreter.print_result x
      in
      Interpreter.eval !env success error prog
    with Interpreter.InterpretationError ->
      print_endline "error while interpreting the program"
  done

