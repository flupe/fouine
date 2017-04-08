open Ast
open Print
open Bytecode
open Structures

exception TypeError
exception ExecutionError

type value
  = UnitVal
  | IntVal of int
  | BoolVal of bool
  | EnvVal of value IncrEnv.t
  | EncapVal of bytecode
  | ClosureVal of identifier * bytecode * value IncrEnv.t

let rec print_value = function
  | UnitVal -> print_endline "()"
  | IntVal i -> print_endline @@ string_of_int i
  | BoolVal b -> print_endline (if b then "true" else "false")
  | _ -> print_endline @@ red "-"

let compute_unary op v = match op, v with
  | Not, BoolVal b -> BoolVal (not b)
  | _ -> raise TypeError

let compute_binary op v v' = match op, v, v' with
  | Plus,  IntVal i,  IntVal j  -> IntVal (i + j)
  | Minus, IntVal i,  IntVal j  -> IntVal (i - j)
  | Mult,  IntVal i,  IntVal j  -> IntVal (i * j)
  | Or,    BoolVal i, BoolVal j -> BoolVal (i || j)
  | And,   BoolVal i, BoolVal j -> BoolVal (i && j)
  | Lt,    IntVal i,  IntVal j  -> BoolVal (i < j)
  | Gt,    IntVal i,  IntVal j  -> BoolVal (i > j)
  | Leq,   IntVal i,  IntVal j  -> BoolVal (i <= j)
  | Geq,   IntVal i,  IntVal j  -> BoolVal (i >= j)
  | Eq,    IntVal i,  IntVal j  -> BoolVal (i = j)
  | Eq,    BoolVal i, BoolVal j -> BoolVal (i = j)
  | Neq,   IntVal i,  IntVal j  -> BoolVal (i <> j)
  | Neq,   BoolVal i, BoolVal j -> BoolVal (i <> j)
  | _ -> raise TypeError

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

    | BinOp op :: c, e, v' :: v :: s ->
        aux (c, e, (compute_binary op v v') :: s)
    
    | Access x :: c, e, s ->
        aux (c, e, (IncrEnv.find x e) :: s)

    | Encap c' :: c, e, s ->
        aux (c, e, (EncapVal c') :: s)

    | Closure (x, c') :: c, e, s ->
        aux (c, e, (ClosureVal (x, c', e)) :: s)

    | Let x :: c, e, v :: s ->
        aux (c, IncrEnv.add x v e, s)

    | EndLet x :: c, e, s ->
        aux (c, IncrEnv.remove x e, s)

    | Apply :: c, e, (ClosureVal (x, c', e')) :: v :: s ->
        aux (c', IncrEnv.add x v e', (EncapVal c) :: (EnvVal e) :: s)

    | Branch :: c, e, (EncapVal cf) :: (EncapVal ct) :: (BoolVal b) :: s ->
        if b then
          aux (ct @ c, e, s)
        else
          aux (cf @ c, e, s)

    | Return :: c, e, v :: (EncapVal c') :: (EnvVal e') :: s ->
        aux (c', e', v :: s)
    
    | Print :: c, e, (IntVal i) :: s ->
        print_int i;
        print_newline ();
        aux (c, e, s)
        
    | [], e, v :: s -> v
    | _, _, s ->
        print_value (List.hd s); 
        raise ExecutionError in

  try
    aux (code, IncrEnv.empty, [])
  with _ ->
    raise ExecutionError