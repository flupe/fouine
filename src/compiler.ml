open Print

(* todo: reuse types from Interpreter? *)
module Env = Map.Make(struct
  type t = Ast.identifier
  let compare = Pervasives.compare
end)

type constant
  = CInt of int
  | CBool of bool
  | CRef of constant ref
  | CClosure of Ast.identifier * Ast.t * constant Env.t
  | CArray of int array
  | CUnit

type instr
  = Access of Ast.identifier
  | Closure of Ast.t
  | Let
  | EndLet
  | Apply
  | Return
  | Add
  | Mult

type prog =
  instr list

let compile ast =
  let rec step acc = function
    | Unit
    | Int i
    |
  in step [] ast

