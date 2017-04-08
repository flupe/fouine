open Ast
open Print
open Structures

let parse_input () =
  try
    Lexing.from_channel stdin
    |> Parser.main Lexer.token
  with _ ->
    print_endline <| magenta "Syntax Error";
    exit 1

let () =
  let debug = ref false in
  let machine = ref false in
  let interm = ref "" in
  let nbe = ref false in

  let speclist =
    [ "-debug", Arg.Set debug, "Display the parsed input."
    ; "-machine", Arg.Set machine, "Compile and run the input from SECD."
    ; "-interm", Arg.Set_string interm, "Compile input to the given output file."
    ; "-NbE", Arg.Set nbe, "Run input with NbE."
    ]

  in Arg.parse speclist (fun x -> ()) "FOUINE INTERPRETOR 2017";

  let env = ref Env.empty in
  while true do
    print_string ">>> ";
    flush stdout;

    let prog = parse_input () in

    if !debug then
      print prog;

    (* compile and run on SECD *)
    if !machine then begin
      let bytecode = Compiler.compile prog in
      print_endline <| Bytecode.string_of_bytecode bytecode;

      (* todo: handle interm *)
      try
        Secd.run bytecode |> Secd.print_value
      with _ ->
        print_endline "error while running the program on the SECD";
    end
    else begin
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
    end
  done
