let (<|) = (@@)

open Ast

exception TypeError
exception InterpretationError
exception ExecutionError
exception MatchError

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

type value
  = CConst of Ast.constant
  | CRef of value ref
  | CMetaClosure of (value -> value)
  | CClosure of Ast.pattern * Ast.t * value Env.t
  | CRec of Ast.identifier * Ast.pattern  * Ast.t * value Env.t
  | CArray of int array
  | CList of value list
  | CTuple of value list

let rec match_pattern env (a : pattern) (b : value) =
  match a, b with
  | PAll, _ -> env
  | PField id, _ ->
      if Env.mem id env then raise MatchError
      else Env.add id b env
  | PConst p, CConst c when p = c -> env
  | PTuple pl, CTuple cl -> match_list env pl cl
  | _ -> raise MatchError

and match_list env al bl = 
  match al, bl with
  | p :: pt, v :: vt -> match_list (match_pattern env p v) pt vt
  | [], [] -> env
  | _ -> raise MatchError

let rec equal_types a b =
  match a, b with
  | CClosure _, CClosure _
  | CRec _, CRec _
  | CList [], CList []
  | CArray _, CArray _ -> true
  | CRef ra, CRef rb -> equal_types !ra !rb
  | CTuple l1, CTuple l2 ->
      List.for_all2 equal_types l1 l2
  | CList (a :: _), CList (b :: _) -> equal_types a b
  | CConst a, CConst b -> begin match a, b with
    | Int _, Int _
    | Bool _, Bool _
    | Unit, Unit -> true
    | _ -> false
    end
  | _ -> false
