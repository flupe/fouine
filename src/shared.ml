let (<|) = (@@)

open Ast
open Bytecode

module Env = Map.Make (struct
  type t = identifier
  let compare = Pervasives.compare
end)

type value
  = CConst of Ast.constant
  | CRef of value ref
  | CArray of int array
  | CList of value list
  | CTuple of value list
  | CMetaClosure of (value -> value)

  | CClosure of Ast.pattern * Ast.t * value Env.t
  | CRec of Ast.identifier * Ast.pattern  * Ast.t * value Env.t

  | CBClosure of Ast.pattern * bytecode * value Env.t
  | CBRec of Ast.identifier * Ast.pattern  * bytecode * value Env.t

exception TypeError
exception MatchError
exception ExecutionError
exception InterpretationError
exception UncaughtError of value

let rec equal_types a b =
  match a, b with
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

type callback = value -> unit

(* generic module type for a toplevel interpreter *)
module type Interp = sig
  (* run an expression and trigger success or error with the result *)
  val eval : callback -> callback -> Ast.t -> unit

  (* bind a new value to some identifier *)
  val bind : id -> value -> unit
end

let rec match_pattern_aux env a b =
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
  | p :: pt, v :: vt -> match_list (match_pattern_aux env p v) pt vt
  | [], [] -> env
  | _ -> raise MatchError

let match_pattern = match_pattern_aux Env.empty

let list_split n =
  let rec aux i = function
    | h :: q when i < n -> let (a, b) = (aux (i + 1) q) in (h :: a, b)
    | a -> [], a
  in aux 0