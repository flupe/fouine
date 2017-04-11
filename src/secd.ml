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
  | RefVal of value ref
  | ArrayVal of value array
  | EnvVal of value IncrEnv.t
  | EncapVal of bytecode
  | ClosureVal of identifier * bytecode * value IncrEnv.t
  | RecClosureVal of identifier * identifier * bytecode * value IncrEnv.t
  | MetaClosureVal of (value -> value)

let rec constant_of_value = function
  | UnitVal    -> CUnit
  | IntVal i   -> CInt i
  | BoolVal b  -> CBool b
  | RefVal r   -> CRef (ref (constant_of_value !r))
  
  | ArrayVal a -> CArray (a |> Array.map 
    (fun x -> match x with
      | IntVal i -> i
      | _ -> raise TypeError))

  | _ -> raise TypeError

let compute_unary op v = match op, v with
  | UMinus, IntVal i -> IntVal ((-1) * i)
  | _ -> raise TypeError

let compute_binary op v v' = match op, v, v' with
  | Plus,   IntVal i,  IntVal j  -> IntVal (i + j)
  | Minus,  IntVal i,  IntVal j  -> IntVal (i - j)
  | Mult,   IntVal i,  IntVal j  -> IntVal (i * j)
  | Div,    IntVal i,  IntVal j  -> IntVal (i / j)
  | Mod,    IntVal i,  IntVal j  -> IntVal (i mod j)
  | Or,     BoolVal i, BoolVal j -> BoolVal (i || j)
  | And,    BoolVal i, BoolVal j -> BoolVal (i && j)
  | Lt,     IntVal i,  IntVal j  -> BoolVal (i < j)
  | Gt,     IntVal i,  IntVal j  -> BoolVal (i > j)
  | Leq,    IntVal i,  IntVal j  -> BoolVal (i <= j)
  | Geq,    IntVal i,  IntVal j  -> BoolVal (i >= j)
  | Eq,     IntVal i,  IntVal j  -> BoolVal (i = j)
  | Eq,     BoolVal i, BoolVal j -> BoolVal (i = j)
  | Neq,    IntVal i,  IntVal j  -> BoolVal (i <> j)
  | Neq,    BoolVal i, BoolVal j -> BoolVal (i <> j)
  | SetRef, RefVal r,  _         -> r := v'; UnitVal
  | _ -> raise TypeError


(** A base environment which contains meta-closures to support "special"
    operations like `ref`, `not`, `prInt` or `aMake`. *)
let base =
  IncrEnv.empty

  |> IncrEnv.add "ref" (MetaClosureVal (fun x ->
      RefVal (ref x)))

  |> IncrEnv.add "not" (MetaClosureVal (fun x -> match x with
      | BoolVal b -> BoolVal (not b)
      | _ -> raise TypeError))

  |> IncrEnv.add "prInt" (MetaClosureVal (fun x -> match x with
      | IntVal i ->
          print_int i;
          print_newline ();
          UnitVal
      | _ -> raise TypeError))

  |> IncrEnv.add "aMake" (MetaClosureVal (fun x -> match x with
      | IntVal i -> ArrayVal (Array.make i (IntVal 0))
      | _ -> raise TypeError))


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

    | Deref :: c, e, RefVal r :: s ->
        aux (c, e, !r :: s)

    | ArraySet :: c, e, v :: IntVal key :: ArrayVal a :: s ->
        a.(key) <- v;
        aux (c, e, UnitVal :: s)

    | ArrayRead :: c, e, IntVal key :: ArrayVal a :: s ->
        aux (c, e, a.(key) :: s)

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

    | RecClosure (f, x, c') :: c, e, s ->
        aux (c, e, (RecClosureVal (f, x, c', e)) :: s)

    | Let x :: c, e, v :: s ->
        aux (c, IncrEnv.add x v e, s)

    | EndLet x :: c, e, s ->
        aux (c, IncrEnv.remove x e, s)

    | Apply :: c, e, (ClosureVal (x, c', e')) :: v :: s ->
        aux (c', IncrEnv.add x v e', (EncapVal c) :: (EnvVal e) :: s)

    | Apply :: c, e, (RecClosureVal (f, x, c', e') as r) :: v :: s ->
        let e'' = IncrEnv.add f r e' in
        aux (c', IncrEnv.add x v e'', (EncapVal c) :: (EnvVal e) :: s)

    | Apply :: c, e, (MetaClosureVal f) :: v :: s ->
        aux (c, e, (f v) :: s)

    | Branch :: c, e, (EncapVal cf) :: (EncapVal ct) :: (BoolVal b) :: s ->
        if b then
          aux (ct @ c, e, s)
        else
          aux (cf @ c, e, s)

    | Return :: c, e, v :: (EncapVal c') :: (EnvVal e') :: s ->
        aux (c', e', v :: s)
        
    | [], e, v :: s -> v
    | _, _, s ->
        raise ExecutionError in

  try
    aux (code, base, [])
  with _ ->
      raise ExecutionError
