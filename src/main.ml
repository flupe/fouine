open Ast
open Print
open Shared
open Beautify

let parse_input () =
  Lexing.from_channel stdin
  |> Parser.main Lexer.token

let expr_success t v =
  Printf.printf "- : %s = %s\n" (Infer.string_of_type t) (Beautify.string_of_value v)

let decl_success t_env p t e =
  let env' = match_pattern p e in
  let t_env' = Infer.match_type 0 [] t p in
  Env.iter (fun name v ->
    Printf.printf "val %s : %s = %s\n" name (Infer.string_of_type <| List.assoc name t_env') (Beautify.string_of_value v)) env';
  Interpreter.append env';
  t_env := t_env' @ !t_env

let error x = 
  print_endline (err "[ERROR]" ^ " Uncaught exception.")

let exec_stmt t_env = function
  | Expr e ->
      let t = Infer.type_of !t_env e in
      Interpreter.exec (expr_success t) error e
  | Decl (p, e) -> 
      let t = Infer.type_of !t_env e in
      Interpreter.exec (decl_success t_env p t) error e

let rec run_prog envs = function
  | s :: t ->
      exec_stmt envs s;
      run_prog envs t
  | _ -> ()

let () =
  let debug = ref false in
  let machine = ref false in
  let no_exceptions = ref false in
  let no_ref = ref false in
  let interm = ref "" in
  let from = ref "" in

  let speclist =
    [ "-debug", Arg.Set debug, "Display the parsed input."
    ; "-machine", Arg.Set machine, "Compile and run the input from SECD."
    ; "-interm", Arg.Set_string interm, "Compile input to the given output file."
    ; "-from", Arg.Set_string from, "Run bytecode from the given output file."
    ; "-E", Arg.Set no_exceptions, "Transform the input to fouine code without exceptions."
    ; "-R", Arg.Set no_ref, "Transform the input to fouine code without references."
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
    let type_env = ref Infer.base_env in

    (* If we do the transformation to get rid of exceptions,
     * we need to add default continuations to the outer scope *)
    (* if !no_exceptions then
      env := !env
        |> Env.add "k" (CMetaClosure (fun x -> print_value x; x))
        |> Env.add "kE" (CMetaClosure (fun x -> error x; x));
        *)

    while true do
      print_string <| bold ">>> ";
      flush stdout;

      (* try *)
        let prog = parse_input () in

        (* if !debug then List.iter print_ast prog; *)

        (* 
        if !no_exceptions then begin
          let prog = prog
            |> List.map Transform.rem_exceptions
            |> List.map (fun x -> Call (x, Tuple [ Var "k"; Var "kE" ]))
          in

          if !debug then List.iter print_ast prog;

          try
            List.iter (fun x -> ignore <| Simpinterp.eval env x) prog
          with Simpinterp.InterpretationError ->
            print_endline <| err "[ERROR]" ^ " The interpreter ended prematurely.";
        end

        else begin *)
          try
            run_prog type_env prog
          with Interpreter.InterpretationError ->
            print_endline <| err "[ERROR]" ^ " The interpreter ended prematurely.";
        (* end *)

      (* with _ ->
        print_endline <| err "[ERROR]" ^ " Syntax error."; *)
    done
    (*
  end
  *)
