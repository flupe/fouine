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
  in

  let combine_prog prog =
    List.fold_right combine_stmt prog (Const Unit)
  in

  (* Compile the input, and output it to a bytecode file. *)
  if !interm <> "" then begin
    let prog = parse_input () in
    let combined = combine_prog prog in
    let _ = Infer.type_of Infer.base_env combined in
    let bytecode = Compiler.compile <| combined in

    if !debug then
      print_endline <| Bytecode.string_of_bytecode bytecode;

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
    with _ ->
      print_endline <| red "[ERROR]" ^ " The SECD machine ended prematurely.";

    close_in chan
  end

  (* Compile the input, and run the bytecode on the SECD machine. *)
  else if !machine then begin
    let prog = parse_input () in
    let combined = combine_prog prog in
    let _ = Infer.type_of Infer.base_env combined in
    let bytecode = Compiler.compile <| combined in

    if !debug then
      print_endline <| Bytecode.string_of_bytecode bytecode;

    try
      Secd.run bytecode |> ignore
    with _ ->
      print_endline <| red "[ERROR]" ^ " The SECD machine ended prematurely.";
  end

  (* Start an interpretation REPL. *)
  else begin
    (* The default type environment. *)
    let t_env = ref Infer.base_env in

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
      | Expr e ->
          let t = Infer.type_of !t_env e in
          Interp.eval (Beautify.log None t) error e
      | Decl (p, e) ->
          let t = Infer.type_of !t_env e in
          let success v = 
            let types = Infer.match_type 0 !t_env t p in
            let values = match_pattern p v in
            let aux id v =
              Beautify.log (Some id) (List.assoc id types) v;
              Interp.bind id v
            in
            Env.iter aux values;
            t_env := types @ !t_env
          in Interp.eval success error e

      | DeclRec (id, e) ->
          let t = Infer.new_var 1 in
          let t_env' = (id, t) :: !t_env in
          Infer.unify t (Infer.type_of t_env' e);
          t_env := (id, Infer.generalize 0 t) :: !t_env;
          let v = CRec (id, e, !Interp.env) in
          Beautify.log (Some id) t v;
          Interp.bind id v
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
        run_prog prog
      with InterpretationError ->
        print_endline <| err "[ERROR]" ^ " The interpreter ended prematurely.";
    done 
  end
