let (<|) = (@@)

open Ast

module Env = Map.Make (struct
  type t = identifier
  let compare = Pervasives.compare
end)

module IncrEnv = struct
  type 'a t = 'a list Env.t

  let empty = Env.empty

  let find x (e : 'a t) =
    List.hd (Env.find x e)

  let add x (v : 'a) (e : 'a t) =
    try
      Env.add x (v :: (Env.find x e)) e
    with _ ->
      Env.add x [v] e

  let remove x (e : 'a t) =
    Env.add x (List.tl (Env.find x e)) e
end

type constant
  = CConst of Ast.constant
  | CRef of constant ref
  | CMetaClosure of (constant -> constant)
  | CClosure of Ast.pattern * Ast.t * constant Env.t
  | CRec of Ast.identifier * Ast.pattern  * Ast.t * constant Env.t
  | CArray of int array
  | CTuple of constant list

let rec equal_types a b =
  match a, b with
  | CClosure _, CClosure _
  | CRec _, CRec _
  | CArray _, CArray _ -> true
  | CRef ra, CRef rb -> equal_types !ra !rb
  | CTuple l1, CTuple l2 ->
      List.for_all2 equal_types l1 l2
  | CConst a, CConst b -> begin match a, b with
    | Int _, Int _
    | Bool _, Bool _
    | Unit, Unit -> true
    | _ -> false
    end
  | _ -> false
