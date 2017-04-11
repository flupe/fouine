open Ast
open Print
open Structures

exception InterpretationError

let eval (env : constant Env.t) gk kE e : unit =
  let k = gk env in
  let rec step env k kE = function
    | Int c -> k <| CInt c
    | Bool b -> k <| CBool b
    | Unit -> k <| CUnit

    | BinaryOp (op, l, r) ->
        let k' lc = 
        let k' rc =
          k <|
          match lc, rc with
          | CInt lv, CInt rv -> begin
              match op with
              | Plus -> CInt (lv + rv)
              | Minus -> CInt (lv - rv)
              | Mult -> CInt (lv * rv)
              | Div -> CInt (lv / rv)
              | Mod -> CInt (lv mod rv)
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
        in step env k' kE r
        in step env k' kE l

    | UnaryOp (op, e) ->
        let k' c =
          k <|
          match c with
          | CBool b ->
              if op = Not then CBool (not b)
              else raise InterpretationError
          | CInt i ->
              if op = UMinus then CInt (-i)
              else raise InterpretationError
          | _ -> raise InterpretationError
        in step env k' kE e

    | Var id ->
        k <|
        if Env.mem id env then
          Env.find id env
        else raise InterpretationError

    | IfThenElse (cond, truthy, falsy) ->
        let k' c = 
          match c with
          | CBool b ->
              if b then step env k kE truthy
              else step env k kE falsy
          | _ -> raise InterpretationError
        in step env k' kE cond

    | LetIn (id, e, fn) ->
        let k' c =
          let env' = Env.add id c env
          in step env' k kE fn
        in step env k' kE e

    | LetRecIn (id, e, fn) -> begin
        match e with
        | Fun (id', e') ->
            let f = CRec(id, id', e', env) in
            let env' = Env.add id f env in
            step env' k kE fn

        (* ain't recursive, or at least not in the way we allow *)
        | _ ->
            let k' c =
              let env' = Env.add id c env
              in step env' k kE fn
            in step env k' kE e
      end

    | Let (id, e) ->
        let k' c =
          let env' = Env.add id c env
          in gk env' c
        in step env k' kE e

    | LetRec (id, e) -> begin
        match e with
        | Fun (id', e') ->
            let f = CRec(id, id', e', env) in
            let env' = Env.add id f env in
            gk env' f

        (* ain't recursive, or at least not in the way we allow *)
        | _ ->
            let k' c =
              let env = Env.add id c env
              in gk env c
            in step env k' kE e
      end

    | Fun (id, e) ->
        k <| CClosure (id, e, env)

    | Call (e, x) ->
        let k' fc =
          match fc with
          | CClosure (id, fn, env') ->
              let k' v = 
                let env' =
                  Env.add id v env'
                in step env' k kE fn
              in step env k' kE x

          | CRec (name, id, e, env') ->
              let k' v = 
                let env' =
                  env'
                  |> Env.add name fc
                  |> Env.add id v
                in step env' k kE e
              in step env k' kE x

          | _ -> raise InterpretationError
        in step env k' kE e

    | Ref e ->
        let k' v =
          k <| CRef (ref v)
        in step env k' kE e

    | Deref e ->
        let k' v =
          match v with
          | CRef r -> k !r
          | _ -> raise InterpretationError
        in step env k' kE e

    | Print e ->
        let k' v = 
          match v with
          | CInt i ->
              print_int i;
              print_newline ();
              k v
          | _ -> raise InterpretationError
        in step env k' kE e

    | ArrayMake e ->
        let k' v = 
          match v with
          | CInt i when i >= 0 ->
              k <| CArray (Array.make i 0)
          | _ -> raise InterpretationError
        in step env k' kE e

    | ArraySet (arr, key, v) ->
        let k' arr = 
          match arr with
          | CArray a ->
              let k' p =
                match p with
                | CInt p when p >= 0 ->
                  if p >= Array.length a then
                    raise InterpretationError
                  else
                  let k' v =
                    match v with
                    | CInt v ->
                        a.(p) <- v;
                        k CUnit
                    | _ -> raise InterpretationError
                  in step env k' kE v
                | _ -> raise InterpretationError
              in step env k' kE key
          | _ -> raise InterpretationError
        in step env k' kE arr

    | ArrayRead (arr, key) ->
        let k' arr =
          match arr with
          | CArray a ->
              let k' p =
                match p with
                | CInt p when p >= 0 ->
                  if p >= Array.length a then
                    raise InterpretationError
                  else
                    k <| CInt a.(p)
                | _ -> raise InterpretationError
              in step env k' kE key
          | _ -> raise InterpretationError
        in step env k' kE arr

    | Raise e ->
        step env kE kE e

    | TryWith (l, p, r) ->
        let kE' x = 
          (* pseudo pattern matching *)
          (* only allowed on ints or catch-all identifiers *)
          match p, x with
            | Int p, CInt v ->
                if p = v then
                  step env k kE r
                else kE x
            | Var id, _ ->
                let env' = Env.add id x env in
                step env' k kE r

            | _ -> raise InterpretationError
        in step env k kE' l

    | Seq (l, r) ->
        let k' lc =
          if lc = CUnit then
            step env k kE r
          else raise InterpretationError
        in step env k' kE l

  in let () = step env k kE e 
  in ()
