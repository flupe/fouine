open Ast
open Print
open Structures
open Beautify

let parse_input () =
  try
    Lexing.from_channel stdin
    |> Parser.main Lexer.token
  with _ ->
    print_endline <| red "[ERROR]" ^ " Invalid syntax.";
    exit 1

let () =
  let debug = ref false in
  let machine = ref false in
  let interm = ref "" in
  let from = ref "" in

  let speclist =
    [ "-debug", Arg.Set debug, "Display the parsed input."
    ; "-machine", Arg.Set machine, "Compile and run the input from SECD."
    ; "-interm", Arg.Set_string interm, "Compile input to the given output file."
    ; "-from", Arg.Set_string from, "Run bytecode from the given output file."
    ]

  in Arg.parse speclist (fun x -> ()) "FOUINE 2017";

  (* Compile the input, and output it to a bytecode file. *)
  if !interm <> "" then begin
    let prog = parse_input () in
    let bytecode = Compiler.compile prog in

    if !debug then
      print_endline <| Bytecode.string_of_bytecode bytecode;

    let chan = open_out_bin !interm in
    Marshal.to_channel chan bytecode [];
    close_out chan
  end

  (* Run a given bytecode file. *)
  else if !from <> "" then begin
    let chan = open_in_bin !from in
    let bytecode = (Marshal.from_channel chan : Bytecode.bytecode) in

    try
      Secd.run bytecode
      |> Secd.constant_of_value
      |> print_constant
    with _ ->
      print_endline <| red "[ERROR]" ^ " The SECD machine ended prematurely.";

    close_in chan
  end

  (* Start a REPL loop. *)
  else begin
    let env = ref Env.empty in
    while true do
      print_string ">>> ";
      flush stdout;

      let prog = parse_input () in

      (* Print the parsed Ast if needed. *)
      if !debug then
        Beautify.print prog;

      (* Compile the input, and run the bytecode on the SECD machine. *)
      if !machine then begin
        let bytecode = Compiler.compile prog in

        if !debug then
          print_endline <| Bytecode.string_of_bytecode bytecode;

        try
          Secd.run bytecode
          |> Secd.constant_of_value
          |> print_constant
        with _ ->
          print_endline <| red "[ERROR]" ^ " The SECD machine ended prematurely.";
      end

      (* Execute the input. *)
      else begin
        try
          let error _ x =
            print_endline <| red "[ERROR]" ^ " Uncaught exception.";
            print_constant x
          in
          let success e x =
            env := e;
            print_constant x
          in
          Interpreter.eval !env success error prog
        with Interpreter.InterpretationError ->
          print_endline <| red "[ERROR]" ^ " The interpreter ended prematurely.";
      end
    done
  end
