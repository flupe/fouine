open Expr

let _ =
  try
    let prog = 
      Lexing.from_channel stdin
      |> Parser.main Lexer.token
    in
    try
      match Interpreter.eval prog with
      | Int i -> print_endline @@ "int : " ^ string_of_int i
      | Bool b -> print_endline @@ "bool : " ^ (if b then "true" else "false")
      | Fun (id, e) ->
          print_string @@ "fun : " ^ id ^ " -> ";
          print_expr e;
          print_newline ()
      | Unit -> print_endline "unit : ()"
    with Interpreter.InterpretationError ->
      print_endline "error while interpreting the program"
  with _ ->
    print_endline "SYNTAX ERROR FDP"

