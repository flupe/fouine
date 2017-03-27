open Ast
open Print

exception InterpretationError

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
let eval e =
  let env = Env.empty in

  let rec step env = function
    | Int c -> CInt c
    | Bool b -> CBool b
    | Unit -> CUnit

    | BinaryOp (op, l, r) -> begin
        let lc = step env l in
        let rc = step env r in

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
      end

    | UnaryOp (op, e) -> begin
        let c = step env e in
        match c with
        | CBool b ->
            if op = Not then CBool (not b)
            else raise InterpretationError
        | _ -> raise InterpretationError
      end

    | Var id ->
        if Env.mem id env then
          Env.find id env
        else raise InterpretationError

    | IfThenElse (cond, truthy, falsy) -> begin
        let c = step env cond in
        match c with
        | CBool b ->
            if b then step env truthy
            else step env falsy
        | _ -> raise InterpretationError
      end

    | Let (id, e, fn) ->
        let c = step env e in
        let env' = Env.add id c env in
        step env' fn

    | LetRec (name, id, e, fn) ->
        let env' = Env.add name (CRec (name, id, e, env)) env in
        step env' fn

    | Fun (id, e) -> CClosure (id, e, env)

    | Call (e, x) -> begin
        let fc = step env e in
        match fc with
        | CClosure (id, fn, env') ->
            let v = step env x in
            let env' =
              Env.add id v env'
            in step env' fn
        | CRec (name, id, fn, env') ->
            let v = step env x in
            let env' =
              env'
              |> Env.add name fc
              |> Env.add id v
            in step env' fn

        | _ -> raise InterpretationError
      end

    | Ref e ->
        let v = step env e in
        CRef (ref v)

    | Deref e -> begin
        let v = step env e in
        match v with
        | CRef r -> !r
        | _ -> raise InterpretationError
      end

    | Print e -> begin
        let v = step env e in
        match v with
        | CInt i ->
            print_int i;
            print_newline ();
            v
        | _ -> raise InterpretationError
      end

    | _ -> CUnit
  in

  step env e

let rec get_type = function
  | CInt _ -> green "int"
  | CBool _ -> yellow "bool"
  (* we enforce non-cyclic references so it can't loop forever *)
  | CRef r -> get_type !r ^ red " ref"
  | CClosure _ -> blue "fun"
  | CRec _ -> blue "rec fun"
  | CUnit -> magenta "unit"

let print_result e =
  let typ = get_type e in
  let print txt = print_endline @@ "- " ^ typ ^ " : " ^ txt in
  match e with
  | CInt i -> print (string_of_int i)
  | CBool b -> print (if b then "true" else "false")
  | CRef r -> print "-"
  | CClosure (id, _, _) -> print (yellow id ^ " -> ast")
  | CRec (name, id, _, _) -> print (yellow name ^ " " ^ yellow id ^ " -> ast")
  | CUnit -> print "()"
