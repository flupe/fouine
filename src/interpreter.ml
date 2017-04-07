open Ast
open Print
open Structures

exception InterpretationError

(* eval : (constant -> 'a) -> (constant -> 'b) -> Ast.t -> unit *)
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
          let env' = Env.add id c env
          in step env' success error fn
        in step env success' error e

    | LetRec (id, e, fn) -> begin
        match e with
        | Fun (id', e') ->
            let f = CRec(id, id', e', env) in
            let env' = Env.add id f env in
            step env' success error fn

        (* ain't recursive, or at least not in the way we allow *)
        | _ ->
            let success' c =
              let env' = Env.add id c env
              in step env' success error fn
            in step env success' error e
      end

    | Fun (id, e) ->
        success <| CClosure (id, e, env)

    | Call (e, x) ->
        let success' fc =
          match fc with
          | CClosure (id, fn, env') ->
              let success' v = 
                let env' =
                  Env.add id v env'
                in step env' success error fn
              in step env success' error x

          | CRec (name, id, e, env') ->
              let success' v = 
                let env' =
                  env'
                  |> Env.add name fc
                  |> Env.add id v
                in step env' success error e
              in step env success' error x

          | _ -> raise InterpretationError
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

    | AMake e ->
        let success' v = 
          match v with
          | CInt i when i >= 0 ->
              CArray (Array.make i 0)
              |> success
          | _ -> raise InterpretationError
        in step env success' error e

    | ArraySet (id, key, v) ->
        if Env.mem id env then
          match (Env.find id env) with
          | CArray a ->
              let success' k =
                match k with
                | CInt k when k >= 0 ->
                  if k >= Array.length a then
                    raise InterpretationError
                  else
                  let success' v =
                    match v with
                    | CInt v ->
                        a.(k) <- v;
                        success CUnit
                    | _ -> raise InterpretationError
                  in step env success' error v
                | _ -> raise InterpretationError
              in step env success' error key
          | _ -> raise InterpretationError
        else raise InterpretationError

    | ArrayRead (id, key) ->
        if Env.mem id env then
          match (Env.find id env) with
          | CArray a ->
              let success' k =
                match k with
                | CInt k when k >= 0 ->
                  if k >= Array.length a then
                    raise InterpretationError
                  else
                    success <| CInt a.(k)
                | _ -> raise InterpretationError
              in step env success' error key
          | _ -> raise InterpretationError
        else raise InterpretationError

    | Raise e ->
        step env error error e

    | TryWith (l, id, r) ->
        let error' x = 
          let env' = Env.add id x env in
          step env' success error r
        in step env success error' l

    | Seq (l, r) ->
        let success' lc =
          if lc = CUnit then
            step env success error r
          else raise InterpretationError
        in step env success' error l

  in let _ = step env success error e 
  in ()

let rec get_type = function
  | CInt _ -> green "int"
  | CBool _ -> yellow "bool"
  (* we enforce non-cyclic references so it can't loop forever *)
  | CRef r -> get_type !r ^ red " ref"
  | CClosure _ -> blue "fun"
  | CRec _ -> blue "rec fun"
  | CArray _ -> cyan "int array"
  | CUnit -> magenta "unit"

let print_result e =
  let print txt = print_endline @@ "- : " ^ get_type e ^ " = " ^ txt in
  match e with
  | CInt i -> print (string_of_int i)
  | CBool b -> print (if b then "true" else "false")
  | CRef r -> print "-"
  | CClosure (id, _, _) -> print (yellow id ^ " -> ast")
  | CRec (name, id, _, _) -> print (yellow name ^ yellow id ^ " -> ast")
  | CArray a ->
      let rec aux acc = function
        | [x] -> acc ^ string_of_int x
        | x :: t -> aux (acc ^ string_of_int x ^ "; ") t
        | _ -> acc
      in let values = aux "" (Array.to_list a)
      in print <| "[| " ^ values ^ " |]"

  | CUnit -> print "()"
