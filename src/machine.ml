open Ast
open Print
open Shared

type value = Shared.value

let make_interp debug = (module struct
  let env = ref Base.base

  let eval k kE e =
    if debug then begin      
      print_endline <| bold "Source AST:";
      Beautify.print_ast e;
      print_newline ();
    end;

    let bytecode = Compiler.compile e in

    if debug then begin      
      print_endline <| bold "Compiled bytecode:";
      print_endline <| Beautify.string_of_bytecode bytecode;
      print_newline ();
    end;

    try
      Secd.run bytecode !env |> k
    with
      | UncaughtError c -> c |> kE
      | _ -> print_endline <| red "[ERROR]" ^ " The SECD machine ended prematurely."

  let bind id v = env := Env.add id v !env
end : Shared.Interp)