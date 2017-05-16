open Ast
open Print
open Shared
open Bytecode

type stack_value
  = SVal of value
  | SEnv of value Env.t
  | SEncap of bytecode

(** run : Bytecode.bytecode -> Shared.Env -> unit
  
  Runs a given SECD bytecode, as specified in the `Bytecode` module.
  Raises an ExecutionError if anything goes wrong.

  The machine uses the following stacks:
  - A code stack, of type `bytecode`.
  - An environment stack, of type `value Env.t list`.
  - A value stack, of type `stack_value list`.
  - An exception handler stack, of type `(pattern * bytecode * value Ent.t) list`.

    Each item in the stack is the combination of a pattern that the exception must
    match in order to be catched, of code that will be executed by the machine if
    the exception is catched, and the environment when the handler was defined. *)
let run code env =
  let rec aux = function
    | BConst c' :: c, e, s, ex ->
        aux (c, e, (SVal (CConst c')) :: s, ex)

    | BTuple n :: c, e, s, ex ->
        let unwrap = function
          | SVal x -> x
          | _ -> raise TypeError in
        let (a, b) = list_split n s in
        aux (c, e, (SVal (CTuple (List.map unwrap a))) :: b, ex)

    | BArray n :: c, e, s, ex ->
        let unwrap = function
          | SVal x -> x
          | _ -> raise TypeError in
        let (a, b) = list_split n s in
        aux (c, e, (SVal (CArray (List.map unwrap a |> Array.of_list))) :: b, ex)

    | BConstructor (id, n) :: c, e, s, ex ->
        let unwrap = function
          | SVal x -> x
          | _ -> raise TypeError in

        let (a, b) = list_split n s in
        let l = List.map unwrap a in

        let params, _ = List.assoc id !Infer.constructors in
        let constr = match l, params with
          | [x], [_] -> CConstructor (id, [x])
          | _, [_] -> CConstructor (id, [CTuple l])
          | _ -> CConstructor (id, l) in

        aux (c, e, (SVal constr) :: b, ex)

    | BArraySet :: c, e, 
      SVal v :: 
      SVal (CConst (Int key)) :: 
      SVal (CArray a) :: s, ex ->
        a.(key) <- v;
        aux (c, e, (SVal (CConst Unit)) :: s, ex)

    | BArrayRead :: c, e, 
      SVal (CConst (Int key)) :: 
      SVal (CArray a) :: s, ex ->
        aux (c, e, (SVal a.(key)) :: s, ex)

    | BAccess x :: c, e :: q, s, ex ->
        aux (c, e :: q, SVal (Env.find x e) :: s, ex)

    | BEncap c' :: c, e, s, ex ->
        aux (c, e, (SEncap c') :: s, ex)

    | BClosure (p, c') :: c, e :: q, s, ex ->
        aux (c, e :: q, (SVal (CBClosure (p, c', e))) :: s, ex)

    | BRecClosure (f, p, c') :: c, e :: q, s, ex ->
        aux (c, e :: q, (SVal (CBRec (f, p, c', e))) :: s, ex)

    | BLet p :: c, e :: q, SVal v :: s, ex ->
        let matched = match_pattern p v in
        let e' = Env.fold Env.add matched e in
        aux (c, e' :: e :: q, s, ex)

    | BEndLet :: c, e :: q, s, ex ->
        aux (c, q, s, ex)

    | BTry p :: c, e, SEncap cf :: SEncap ct :: s, ex ->
        aux (ct @ c, e, s, (p, cf @ c, e) :: ex)

    | BRaise :: c, e, SVal v :: s, ex ->
        let rec handle = function
          | [] -> raise (UncaughtError v)
          | (p, c', e' :: eq) :: q ->
              begin try
                let matched = match_pattern p v in
                let e'' = Env.fold Env.add matched e' in
                aux (c', e'' :: eq, s, q)
              with 
                _ -> handle q
              end
          | _ -> raise ExecutionError
        in handle ex

    | BApply :: c, e :: q, SVal (CBClosure (p, c', e')) :: SVal v :: s, ex ->
        let matched = match_pattern p v in
        let e'' = Env.fold Env.add matched e' in

        aux (c', e'' :: q, SEncap c :: SEnv e :: s, ex)

    | BApply :: c, e :: q, SVal (CBRec (f, p, c', e') as r) :: SVal v :: s, ex ->
        let matched = match_pattern p v in
        let e'' = Env.fold Env.add matched e' in
        aux (c', (Env.add f r e'') :: q, SEncap c :: SEnv e :: s, ex)

    | BApply :: c, e, SVal (CMetaClosure f) :: SVal v :: s, ex ->
        aux (c, e, SVal (f v) :: s, ex)

    | BBranch :: c, e, SEncap cf :: SEncap ct :: SVal (CConst (Bool b)) :: s, ex ->
        if b then
          aux (ct @ c, e, s, ex)
        else
          aux (cf @ c, e, s, ex)

    | BReturn :: c, e :: q, v :: SEncap c' :: SEnv e' :: s, ex ->
        aux (c', e' :: q, v :: s, ex)
        
    | [], e :: q, SVal v :: s, ex -> v
    | _, _, _, _ -> CConst (Unit)
    | c, _, _, _ -> Beautify.string_of_bytecode c |> print_endline; failwith "wtf" in

  aux (code, [env], [], [])
