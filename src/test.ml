open Ast
open Print
open Shared

type value = Shared.value

let rec quasi_equal v v' = match v, v' with
  | CClosure _, CBClosure _ -> true
  | CRec _, CBRec _ -> true
  | _ -> v = v'

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
          if quasi_equal v v' then
            print_endline <| green "[AUTOTEST] The two output values are equal."
          else
            print_endline <| err "[AUTOTEST] The two output values differ.";
            
          k (CTuple [v; v'])) kE e) kE e

    let bind id = function
      | CTuple [v; v'] ->
          IInterp.bind id v;
          MInterp.bind id v'
      | _ -> failwith "This is not supposed to happen."
  end : Shared.Interp)