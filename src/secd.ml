open Ast
open Print
open Shared
open Bytecode

type stack_value
  = SVal of value
  | SEnv of value Env.t
  | SEncap of bytecode

(** run : Bytecode.bytecode -> unit
  
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
let run code =
  let rec aux = function
    | BConst c' :: c, e, s ->
        aux (c, e, (SVal (CConst c')) :: s)

    | BTuple n :: c, e, s ->
        let unwrap = function
          | SVal x -> x
          | _ -> raise TypeError in
        let (a, b) = list_split n s in
        aux (c, e, (SVal (CTuple (List.map unwrap a))) :: b)

    | BArraySet :: c, e, 
      SVal (CConst (Int v)) :: 
      SVal (CConst (Int key)) :: 
      SVal (CArray a) :: s ->
        a.(key) <- v;
        aux (c, e, (SVal (CConst Unit)) :: s)

    | BArrayRead :: c, e, 
      SVal (CConst (Int key)) :: 
      SVal (CArray a) :: s ->
        aux (c, e, (SVal (CConst (Int a.(key)))) :: s)

    | BAccess x :: c, e :: q, s ->
        aux (c, e :: q, SVal (Env.find x e) :: s)

    | BEncap c' :: c, e, s ->
        aux (c, e, (SEncap c') :: s)

    | BClosure (p, c') :: c, e :: q, s ->
        aux (c, e :: q, (SVal (CBClosure (p, c', e))) :: s)

    | BRecClosure (f, p, c') :: c, e :: q, s ->
        aux (c, e :: q, (SVal (CBRec (f, p, c', e))) :: s)

    | BLet p :: c, e :: q, SVal v :: s ->
        let matched = match_pattern p v in
        let e' = Env.fold Env.add matched e in
        aux (c, e' :: e :: q, s)

    | BEndLet :: c, e :: q, s ->
        aux (c, q, s)

    (* todo: exception handling *)

    | BApply :: c, e :: q, SVal (CBClosure (p, c', e')) :: SVal v :: s ->
        let matched = match_pattern p v in
        let e'' = Env.fold Env.add matched e' in

        aux (c', e'' :: q, SEncap c :: SEnv e :: s)

    | BApply :: c, e :: q, SVal (CBRec (f, p, c', e') as r) :: SVal v :: s ->
        let matched = match_pattern p v in
        let e'' = Env.fold Env.add matched e' in
        aux (c', (Env.add f r e'') :: q, SEncap c :: SEnv e :: s)

    | BApply :: c, e, SVal (CMetaClosure f) :: SVal v :: s ->
        aux (c, e, SVal (f v) :: s)

    | BBranch :: c, e, SEncap cf :: SEncap ct :: SVal (CConst (Bool b)) :: s ->
        if b then
          aux (ct @ c, e, s)
        else
          aux (cf @ c, e, s)

    | BReturn :: c, e :: q, v :: SEncap c' :: SEnv e' :: s ->
        aux (c', e' :: q, v :: s)
        
    | [], e :: q, SVal v :: s -> v
    | [], _, _ -> CConst (Unit)
    | _ -> raise ExecutionError in

  try
    aux (code, [Base.base], [])
  with _ ->
      raise ExecutionError
