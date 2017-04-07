open Ast
open Bytecode
open Structures

exception ExecutionError

type value
  = UnitVal
  | IntVal of int
  | BoolVal of bool
  | EnvVal of value Env.t
  | EncapVal of bytecode
  | ClosureVal of identifier * bytecode * value Env.t

(** run : Bytecode.bytecode -> unit
  
  Runs a given SECD bytecode, as specified in the `Bytecode` module.
  Raises an ExecutionError if anything goes wrong. *)
let run code =
  let rec aux = function
    | UnitConst :: c, e, s ->
        aux (c, e, UnitVal :: s)

    | IntConst i :: c, e, s ->
        aux (c, e, IntVal i :: s)

    | BoolConst b :: c, e, s ->
        aux (c, e, BoolVal b :: s)

    | UnOp op :: c, e, v :: s ->
        aux (c, e, (compute_unary op v) :: s)

    | BinOp op :: c, e, v :: v' :: s ->
        aux (c, e, (compute_binary op v v') :: s)
    
    | Access x :: c, e, s ->
        aux (c, e, (IncrEnv.find e x) :: s)

    | Encap c' :: c, e, s ->
        aux (c, e, (EncapVal c') :: s)

    | Closure (x, c') :: c, e, s ->
        aux (c, e, (ClosureVal (x, c', e)) :: s)

    | Let x :: c, e, v :: s ->
        aux (c, IncrEnv.add e x v, s)

    | EndLet x :: c, e, s ->
        aux (c, IncrEnv.remove e x, s)

    
    | Apply :: c, e, (ClosureVal (x, c', e')) :: v :: s ->
        aux (c', IncrEnv.add (e' x v), (EncapVal c) :: (EnvVal e) :: s)

    | Branch :: c, e, (BoolVal b) :: (EncapVal ct) :: (EncapVal cf) :: s ->
        if b then
          aux (ct @ c, e, v)
        else
          aux (cf @ c, e, v)

    | Return :: c, e, v :: (EncapVal c') :: (EnvVal e') :: s ->
        aux (c', e', v :: s)
    
    | Print :: c, e, (IntVal i) :: s ->
        print_string "Printed Integer: ";
        print_int i;
        print_newline ();
        aux (c, e, s)
        
    | [], e, v :: s -> v
    | _ ->
        raise ExecutionError in

  try
    aux (code, IncrEnv.empty, [])
  with _ ->
    raise ExecutionError