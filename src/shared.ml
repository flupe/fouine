let (<|) = (@@)

open Ast
open Bytecode

module Env = Map.Make (struct
  type t = identifier
  let compare = Pervasives.compare
end)

(*module IncrEnv = struct
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
end*)

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
      if Env.mem id env then failwith "matching error"
      else Env.add id b env
  | PConst p, CConst c when p = c -> env
  | PTuple pl, CTuple cl -> match_list env pl cl
  | _ -> failwith "matching error"

and match_list env al bl = 
  match al, bl with
  | p :: pt, v :: vt -> match_list (match_pattern env p v) pt vt
  | [], [] -> env
  | _ -> failwith "matching error"

let rec equal_types a b =
  match a, b with
  | `CBClosure _, `CBClosure _
  | `CBRec _, `CBRec _
  | `CClosure _, `CClosure _
  | `CRec _, `CRec _
  | `CList [], `CList []
  | `CArray _, `CArray _ -> true
  | `CRef ra, `CRef rb -> equal_types !ra !rb
  | `CTuple l1, `CTuple l2 ->
      List.for_all2 equal_types l1 l2
  | `CList (a :: _), `CList (b :: _) -> equal_types a b
  | `CConst a, `CConst b -> begin match a, b with
    | Int _, Int _
    | Bool _, Bool _
    | Unit, Unit -> true
    | _ -> false
    end
  | _ -> false

let base =
  List.fold_left (fun e (id, v) -> Env.add id v e) Env.empty <|
    [ "ref", meta (fun x -> `CRef (ref x))
    ; "incr", meta (function
        | `CRef ({ contents = `CConst (Int i)} as r) -> r := `CConst (Int (i + 1)); `CConst Unit
        | _ -> raise TypeError)
    ; "decr", meta (function
        | `CRef ({ contents = `CConst (Int i)} as r) -> r := `CConst (Int (i - 1)); `CConst Unit
        | _ -> raise TypeError)
    ; "not", meta (function `CConst (Bool b) -> `CConst (Bool (not b)) | _ -> raise TypeError)
    ; "prInt", meta (function `CConst (Int i) as x -> print_endline <| string_of_int i; x | _ -> raise TypeError)
    ; "prOut", meta (fun x -> (); `CConst Unit)
    ; "aMake", meta (function `CConst (Int n) when n >= 0 -> `CArray (Array.make n 0) | _ -> raise TypeError)

    ; "!", meta (function `CRef x -> !x | _ -> raise TypeError)
    ; ":=", meta (function 
        | `CRef r -> meta (fun x -> if equal_types x !r then (r := x; `CConst Unit) else raise TypeError)
        | _ -> raise TypeError)
    ; "+", int_binop (+)
    ; "-", int_binop (-)
    ; "~-", meta (function `CConst (Int x) -> `CConst (Int ~-x) | _ -> raise TypeError)
    ; "*", int_binop ( * )
    ; "/", int_binop (/)
    ; "mod", int_binop (mod)
    ; "<", gen_bool_binop (<)
    ; "<=", gen_bool_binop (<=)
    ; ">", gen_bool_binop (>)
    ; ">=", gen_bool_binop (>=)
    ; "=", gen_bool_binop (=)
    ; "<>", gen_bool_binop (<>)
    ; "&&", bool_binop (&&)
    ; "||", bool_binop (||)
    ; "|>", meta (fun x -> meta (function `CMetaClosure f -> f x | _ -> raise TypeError))
    ; "@@", meta (function `CMetaClosure f -> meta (fun x -> f x) | _ -> raise TypeError)
    ; "::", meta (fun x -> meta (function
        | `CList []  -> `CList [x]
        | `CList ((a :: _) as t) ->
            if equal_types a x then `CList (x :: t)
            else raise TypeError
        | _ -> raise TypeError))
    ; "@", meta (fun a -> meta (fun b ->
        match a, b with
        | `CList ([] as a), `CList b
        | `CList a, `CList ([] as b) -> `CList (a @ b)
        | `CList ((a :: _) as ta), `CList ((b :: _) as tb) ->
            if equal_types a b then `CList (ta @ tb)
            else raise TypeError
        | _ -> raise TypeError))
    ]

type callback = value -> unit

(* generic module type for a toplevel interpreter *)
module type Interp = sig
  type value

  (* run an expression and trigger success or error with the result *)
  val exec : prog -> callback -> callback -> unit

  (* append values to the current environment *)
  val append : value Env.t -> unit
end


let rec match_pattern_aux env a b =
  match a, b with
  | `PAll, _ -> env
  | `PField id, _ ->
      if Env.mem id env then raise MatchError
      else Env.add id b env
  | `PConst p, `CConst c when p = c -> env
  | `PTuple pl, `CTuple cl -> match_list env pl cl
  | _ -> raise MatchError

and match_list env al bl = 
  match al, bl with
  | p :: pt, v :: vt -> match_list (match_pattern_aux env p v) pt vt
  | [], [] -> env
  | _ -> raise MatchError

let match_pattern = match_pattern_aux Env.empty
