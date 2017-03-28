open Ast
open Print

exception InterpretationError

let (<|) = (@@)

module Env = Map.Make(struct
  type t = Ast.identifier
  let compare = Pervasives.compare
end)

type constant
  = CInt of int
  | CBool of bool
  | CRef of constant ref
  | CClosure of Ast.identifier * Ast.t * constant Env.t
  | CRec of Ast.identifier * Ast.identifier * Ast.t * constant Env.t
  | CUnit

let rec equal_types a b =
  match a, b with
  | CInt _, CInt _
  | CBool _, CBool _
  | CClosure _, CClosure _
  | CRec _, CRec _
  | CUnit, CUnit -> true
  | CRef ra, CRef rb ->
      equal_types !ra !rb
  | _ -> false

(* eval : expr -> constant *)
let eval success error e =
  let env = Env.empty in

  let rec step env success error = function
    | Int c -> success <| CInt c
    | Bool b -> success <| CBool b
    | Unit -> success <| CUnit

    | BinaryOp (op, l, r) ->
        let success' lc = 
          let success' rc =
            success <|
            match lc, rc with
            | CInt lv, CInt rv -> begin
                match op with
                | Plus -> CInt (lv + rv)
                | Minus -> CInt (lv - rv)
                | Mult -> CInt (lv * rv)
                | Lt -> CBool (lv < rv)
                | Gt -> CBool (lv > rv)
                | Leq -> CBool (lv <= rv)
                | Geq -> CBool (lv >= rv)
                | Eq -> CBool (lv = rv)
                | Neq -> CBool (lv <> rv)
                | _ -> raise InterpretationError
              end

            | CBool lv, CBool rv -> begin
                match op with
                | Or -> CBool (lv || rv)
                | And -> CBool (lv && rv)
                | Eq -> CBool (lv = rv)
                | Neq -> CBool (lv <> rv)
                | _ -> raise InterpretationError
              end

            | CRef r, _ ->
                if op = SetRef then
                  (* references cannot change type *)
                  if equal_types rc !r then begin
                    r := rc;
                    CUnit
                  end else raise InterpretationError
                else raise InterpretationError

            | _ -> raise InterpretationError
          in step env success' error r
        in step env success' error l

    | UnaryOp (op, e) ->
        let success' c =
          success <|
          match c with
          | CBool b ->
              if op = Not then CBool (not b)
              else raise InterpretationError
          | _ -> raise InterpretationError
        in step env success' error e

    | Var id ->
        success <|
        if Env.mem id env then
          Env.find id env
        else raise InterpretationError

    | IfThenElse (cond, truthy, falsy) ->
        let success' c = 
          match c with
          | CBool b ->
              if b then step env success error truthy
              else step env success error falsy
          | _ -> raise InterpretationError
        in step env success' error cond

    | Let (id, e, fn) ->
        let success' c =
          let env' = Env.add id c env in
          step env' success error fn
        in step env success' error e

    | LetRec (name, id, e, fn) ->
        let env' = Env.add name (CRec (name, id, e, env)) env in
        step env' success error fn

    | Fun (id, e) ->
        success <| CClosure (id, e, env)

    | Call (e, x) ->
        let success' fc =
          (* todo: refactor to compute x only if we do have a function *)
          let success' v = 
            match fc with
            | CClosure (id, fn, env') ->
                let env' =
                  Env.add id v env'
                in step env' success error fn
            | CRec (name, id, fn, env') ->
                let env' =
                  env'
                  |> Env.add name fc
                  |> Env.add id v
                in step env' success error fn
            | _ -> raise InterpretationError
          in step env success' error x
        in step env success' error e

    | Ref e ->
        let success' v =
          success <| CRef (ref v)
        in step env success' error e

    | Deref e ->
        let success' v =
          match v with
          | CRef r -> success !r
          | _ -> raise InterpretationError
        in step env success' error e

    | Print e ->
        let success' v = 
          match v with
          | CInt i ->
              print_int i;
              print_newline ();
              success v
          | _ -> raise InterpretationError
        in step env success' error e

    | Raise e ->
        step env error error e

    | TryWith (l, id, r) ->
        let error' x = 
          let env' = Env.add id x env in
          step env' success error r
        in step env success error' l

    (* | Seq (l, r) ->
        let lc = step env l in
        if lc = CUnit then
          step env r
        else
          raise InterpretationError *)

    | _ -> success CUnit
  in

  step env success error e

let rec get_type = function
  | CInt _ -> green "int"
  | CBool _ -> yellow "bool"
  (* we enforce non-cyclic references so it can't loop forever *)
  | CRef r -> get_type !r ^ red " ref"
  | CClosure _ -> blue "fun"
  | CRec _ -> blue "rec fun"
  | CUnit -> magenta "unit"

let print_result e =
  let print txt = print_endline @@ "- : " ^ get_type e ^ " = " ^ txt in
  match e with
  | CInt i -> print (string_of_int i)
  | CBool b -> print (if b then "true" else "false")
  | CRef r -> print "-"
  | CClosure (id, _, _) -> print (yellow id ^ " -> ast")
  | CRec (name, id, _, _) -> print (yellow name ^ " " ^ yellow id ^ " -> ast")
  | CUnit -> print "()"
