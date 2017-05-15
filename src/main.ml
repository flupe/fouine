open Ast
open Print
open Shared
open Secd

let parse_input () =
  try
    Lexing.from_channel stdin
    |> Parser.main Lexer.token
  with _ ->
    print_endline <| err "[ERROR]" ^ " Syntax error.";
    exit 0

let error _ = 
  print_endline <| err "[ERROR]" ^ " Uncaught exception."

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

  (* Combine a given program into a single expression. *)
  let combine_stmt a b = match a with
    | Expr e -> Let (PAll, e, b)
    | Decl (p, e) -> Let (p, e, b)
    | DeclRec (id, e) -> LetRec (id, e, b)
    | _ -> b
  in

  let combine_prog prog =
    List.fold_right combine_stmt prog (Const Unit)
  in

  let shit_prog = function
    | Expr e :: q -> e
    | _ -> raise UnsupportedError
  in

  if !interm <> "" then begin
    let prog = parse_input () in
    let combined = combine_prog prog in
    let _ = Infer.type_of !Infer.env combined in
    let bytecode = Compiler.compile <| combined in

    if !debug then begin
      print_newline ();
      print_endline <| bold "Formatted AST:";
      Beautify.print_ast combined;
      print_newline ();
      print_endline <| bold "Compiled bytecode:";
      print_endline <| Bytecode.string_of_bytecode bytecode;
    end;

    let chan = open_out_bin !interm in
    List.iter (fun bytes -> Marshal.to_channel chan bytes []) bytecode;
    close_out chan
  end

  (* Run a given bytecode file. *)
  else if !from <> "" then begin
    let chan = open_in_bin !from in
    let bytecode = (Marshal.from_channel chan : Bytecode.bytecode) in

    try
      Secd.run bytecode |> ignore
    with
      | UncaughtError c -> print_endline <| red "[ERROR]" ^ " Uncaught exception: " ^ Beautify.string_of_value c ^ "."
      | _ -> print_endline <| red "[ERROR]" ^ " The SECD machine ended prematurely.";

    close_in chan
  end

  (* Compile the input, and run the bytecode on the SECD machine. *)
  else if !machine then begin
    let prog = parse_input () in
    let combined = combine_prog prog in
    let _ = Infer.type_of !Infer.env combined in
    let bytecode = Compiler.compile <| combined in

    if !debug then begin
      print_newline ();
      print_endline <| bold "Formatted AST:";
      Beautify.print_ast combined;
      print_newline ();
      print_endline <| bold "Compiled bytecode:";
      print_endline <| Bytecode.string_of_bytecode bytecode;
      print_newline ();
      print_endline <| bold "Standard output:";
    end;

    try
      Secd.run bytecode |> ignore
    with
      | UncaughtError c -> print_endline <| red "[ERROR]" ^ " Uncaught exception: " ^ Beautify.string_of_value c ^ "."
      | _ -> print_endline <| red "[ERROR]" ^ " The SECD machine ended prematurely.";
  end

  (* Start an interpretation REPL. *)
  else begin
    (* We fetch the module with which to interpret our input. *)
    let (module Interp) = begin
      (* if we plan on doing transformartions on our code
       * we use a simpler interpreter *)
      if !no_exceptions || !no_ref then
        Simpinterp.make_interp !no_exceptions !no_ref
      else
        (module Interpreter : Shared.Interp)
    end in

    (* Execute a given statement. *)
    let rec exec_stmt = function
      | Expr e as s ->
          let t = Infer.type_of_stmt s in
          Interp.eval (Beautify.log None t) error e

      | Decl (p, e) as s ->
          ignore <| Infer.type_of_stmt s;
          let success v = 
            let values = match_pattern p v in
            let aux id v =
              Beautify.log (Some id) (List.assoc id !Infer.env) v;
              Interp.bind id v
            in
            Env.iter aux values;
          in Interp.eval success error e

      | DeclRec (id, e) as s ->
          let e = LetRec (id, e, Var id) in
          let t = Infer.type_of_stmt s in
          let success v =
            Beautify.log (Some id) t v;
            Interp.bind id v
          in
          Interp.eval success error e

      | TypeDef (params, name, decl) -> Infer.declare_type name params decl
      in

    let rec run_prog = function
      | s :: t ->
          exec_stmt s;
          run_prog t
      | [] -> ()
    in

    (* Let the fun begin. *)
    while true do
      print_string <| bold ">>> ";
      flush stdout;

      let prog = parse_input () in
      try
        let combined = shit_prog prog in
        combined
        |> Beautify.print_ast;

        combined
        |> Transform.rem_ref
        |> Beautify.print_ast;

        Expr (combined
        |> Transform.rem_ref)
        |> exec_stmt;

      (*try
        run_prog prog*)
      with InterpretationError ->
        print_endline <| err "[ERROR]" ^ " The interpreter ended prematurely.";
    done 
  end
