open Ast
open Print
open Shared

type value = Shared.value

let make_interp debug =
  let (module IInterp) = Interpreter.make_interp false in
  let (module MInterp) = Machine.make_interp false in

  (module struct
    let eval k kE e =
      if debug then begin
        print_endline <| bold "Source AST:";
        Beautify.print_ast e;
        print_newline ();
      end;

      print_endline <| cyan "[AUTOTEST] Here begins the interpreter.";
      IInterp.eval (fun v -> 
        print_endline <| cyan "[AUTOTEST] Here begins the SECD machine.";
        MInterp.eval (fun v' ->
          if v = v' then begin
            print_endline <| green "[AUTOTEST] The two output values are equal.";
            k v
          end else begin
            print_endline <| err "[AUTOTEST] The two output values differ.";
            k v;
            k v'
          end) kE e) kE e

    let bind id v = 
      IInterp.bind id v;
      MInterp.bind id v
  end : Shared.Interp)