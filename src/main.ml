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

(* default formatting for error messages *)
let error _ = 
  print_endline <| err "[ERROR]" ^ " Uncaught exception."

let () =
  let debug = ref false in
  let machine = ref false in
  let no_exceptions = ref false in
  let no_ref = ref false in
  let interm = ref "" in
  let source = ref "" in
  let from = ref "" in

  let speclist =
    [ "-debug", Arg.Set debug, "Display the parsed input."
    ; "-machine", Arg.Set machine, "Use the SECD instead of the standard Interpreter."
    ; "-interm", Arg.Set_string interm, "Compile input to the given output file."
    ; "-from", Arg.Set_string from, "Run bytecode from the given output file."
    ; "-E", Arg.Set no_exceptions, "Transform the input to fouine code without exceptions."
    ; "-R", Arg.Set no_ref, "Transform the input to fouine code without references."
    ]

  in Arg.parse speclist ((:=) source) "Fouine REPL 2017";

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

  (* Compile to a given bytecode file. *)
  if !interm <> "" then begin
    let prog = parse_input () in
    let combined = combine_prog prog in
    let _ = Infer.type_of !Infer.env combined in
    let bytecode = Compiler.compile <| combined in

    if !debug then begin
      print_newline ();
      print_endline <| bold "Source AST:";
      Beautify.print_ast combined;
      print_newline ();
      print_endline <| bold "Compiled bytecode:";
      print_endline <| Beautify.string_of_bytecode bytecode;
    end;

    let chan = open_out_bin !interm in
    Marshal.to_channel chan bytecode [];
    close_out chan
  end

  (* Run a given bytecode file. *)
  else if !from <> "" then begin
    let chan = open_in_bin !from in
    let bytecode = (Marshal.from_channel chan : Bytecode.bytecode) in

    if !debug then begin
      print_newline ();
      print_endline <| bold "Input bytecode:";
      print_endline <| Beautify.string_of_bytecode bytecode;
    end;

    try
      Secd.run bytecode Base.base |> ignore
    with
      | UncaughtError c -> print_endline <| red "[ERROR]" ^ " Uncaught exception: " ^ Beautify.string_of_value c ^ "."
      | _ -> print_endline <| red "[ERROR]" ^ " The SECD machine ended prematurely.";

    close_in chan
  end

  (* Start an interpretation REPL. *)
  else begin
    (* We fetch the module with which to interpret our input. *)
    let (module Interp) = begin
      (* If we're doing code transformations, we're using a simpler interpreter. *)
      if !no_exceptions || !no_ref then
        Simpinterp.make_interp !debug !no_exceptions !no_ref
      else if !machine then
        Machine.make_interp !debug        
      else
        Interpreter.make_interp !debug
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

      | TypeDef (params, name, decl) ->
          Infer.declare_type name params decl;
          print_endline ("Type " ^ name ^ " was defined.")

    in

    let rec run_prog = function
      | s :: t ->
          exec_stmt s;
          run_prog t
      | [] -> ()
    in

    (* A source file was provided, we execute it first *)
    if !source <> "" then begin
      let parse_file file_name =
        let lexing = Lexing.from_channel @@ open_in file_name in
        let rec aux () = 
          try
            let code = lexing |> Parser.main Lexer.token
            in
            if code = [] then code
            else code @ aux ()
          with _ ->
            print_endline <| err "[ERROR]" ^ " Syntax error.";
            exit 0
        in
        aux ()
      in
      let prog = parse_file !source in

      try
        run_prog prog
      with InterpretationError ->
        print_endline <| err "[ERROR]" ^ " The interpreter ended prematurely.";
    end;

    (* Let the fun begin. *)
    while true do
      print_string <| bold ">>> ";
      flush stdout;

      let prog = parse_input () in

      try
        run_prog prog
      with InterpretationError ->
        print_endline <| err "[ERROR]" ^ " The interpreter ended prematurely.";
    done 
  end
