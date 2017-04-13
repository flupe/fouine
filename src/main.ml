open Ast
open Print
open Shared
open Beautify

let parse_input () =
  Lexing.from_channel stdin
  |> Parser.main Lexer.token

let rec run_prog env success error = function
  | e :: t ->
      Interpreter.eval !env success error e;
      run_prog env success error t
  | _ -> ()

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

  in Arg.parse speclist ignore "Fouine REPL 2017";

  (*
  let prog = parse_input () in
  let env = Interpreter.base in
  let error x =
    print_endline (red "[ERROR]" ^ " Uncaught exception.");
    print_value x in
  let success e x =
    print_value x in
  try
    Interpreter.eval env success error (List.hd prog)
  with Interpreter.InterpretationError ->
    print_endline (red "[ERROR]" ^ " The interpreter ended prematurely.")


  (* Compile the input, and output it to a bytecode file. *)
  if !interm <> "" then begin
    print_string <| bold "∴ ";
    flush stdout;

    let prog = parse_input () in
    let bytecode = List.map Compiler.compile prog in

    if !debug then
      List.iter (fun x -> print_endline <| Bytecode.string_of_bytecode x) bytecode;

    let chan = open_out_bin !interm in
    List.iter (fun bytes -> Marshal.to_channel chan bytes []) bytecode;
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

  (* Compile the input, and run the bytecode on the SECD machine. *)
  else if !machine then begin
    try
      print_string <| bold "∴ ";
      flush stdout;
      let prog = parse_input () in
      let bytecode = List.fold_right (@) (List.map Compiler.compile prog) [] in

      if !debug then
        List.iter print_ast prog;
        print_endline <| Bytecode.string_of_bytecode bytecode;

      try
        Secd.run bytecode
        |> Secd.constant_of_value
        |> print_constant
      with _ ->
        print_endline <| red "[ERROR]" ^ " The SECD machine ended prematurely.";
    with _ ->
      print_endline <| red "[ERROR]" ^ " Syntax error.";
  end

  (* Start an interpretation REPL. *)
  else begin *)
    let env = ref Interpreter.base in

    while true do
      (* ilé bien gentil romain mais 
       * j'ai pas ce caractère dans ma police *)
      print_string <| bold ">>> ";
      flush stdout;

      try
        let prog = parse_input () in

        if !debug then
          List.iter print_ast prog;

        let error x =
          print_endline <| red "[ERROR]" ^ " Uncaught exception.";
          print_value x in

        let success e x =
          env := e;
          print_value x in

        try
          run_prog env success error prog
        with Interpreter.InterpretationError ->
          print_endline <| red "[ERROR]" ^ " The interpreter ended prematurely.";

      with _ ->
        print_endline <| red "[ERROR]" ^ " Syntax error.";
    done
    (*
  end
  *)
