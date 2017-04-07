let (<|) = (@@)

module Env = Map.Make (struct
  type t = Ast.identifier
  let compare = Pervasives.compare
end)

module IncrEnv = Map.Make (struct
  type t = Ast.identifier
  let compare = Pervasives.compare
end)

type constant
  = CInt of int
  | CBool of bool
  | CRef of constant ref
  | CClosure of Ast.identifier * Ast.t * constant Env.t
  | CRec of Ast.identifier * Ast.identifier * Ast.t * constant Env.t
  | CArray of int array
  | CUnit

let rec equal_types a b =
  match a, b with
  | CInt _, CInt _
  | CBool _, CBool _
  | CClosure _, CClosure _
  | CRec _, CRec _
  | CArray _, CArray _
  | CUnit, CUnit -> true
  | CRef ra, CRef rb -> equal_types !ra !rb
  | _ -> false

type 'a callback =
  constant Env.t -> constant -> unit