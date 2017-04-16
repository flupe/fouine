open Ast
open Print
open Shared

exception InterpretationError
exception TypeError

(* the default environment
 * contains our builtin functions *)
let base = Env.empty
  |> Env.add "ref"   (CMetaClosure (fun x -> CRef (ref x)))
  |> Env.add "not"   (CMetaClosure (fun x ->
       match x with
       | CConst (Bool b) -> CConst (Bool (not b))
       | _ -> raise TypeError
     ))
  |> Env.add "prInt" (CMetaClosure (fun x ->
       match x with
       | CConst (Int i) -> print_endline <| string_of_int i; x
       | _ -> raise TypeError
     ))
  |> Env.add "prOut" (CMetaClosure (fun x -> 
       Beautify.print_value x;
       CConst Unit
     ))
  |> Env.add "aMake" (CMetaClosure (fun n ->
       match n with
       | CConst (Int n) when n >= 0 -> CArray (Array.make n 0)
       | _ -> raise TypeError
     ))

let rec match_pattern env (a : pattern) (b : constant) =
  let rec aux penv a b = match a, b with
  | PAll, _ -> true, env
  | PField id, _ ->
      if Env.mem id penv then
        if Env.find id penv = b then true, penv
        else false, env
      else true, Env.add id b penv
  | PConst p, CConst c -> p = c, env
  | PPair (ap, bp), CTuple (av, bv) ->
      let matched, penv = match_pattern penv ap av in
      if matched then
        let matched, penv = match_pattern penv bp bv in
        if matched then true, penv 
        else false, env
      else false, env
  | _ -> raise InterpretationError
  in aux env a b

let eval (env : constant Env.t) gk kE e : unit =
  let k = gk env in
  let rec step env k kE = function
    | Const c -> k <| CConst c

    | BinaryOp (op, l, r) ->
        let k' lc = 
        let k' rc =
          k <|
          match lc, rc with
          | CConst (Int lv), CConst (Int rv) -> begin
              match op with
              | Plus -> CConst (Int (lv + rv))
              | Minus -> CConst (Int (lv - rv))
              | Mult -> CConst (Int (lv * rv))
              | Div -> CConst (Int (lv / rv))
              | Mod -> CConst (Int (lv mod rv))
              | Lt -> CConst (Bool (lv < rv))
              | Gt -> CConst (Bool (lv > rv))
              | Leq -> CConst (Bool (lv <= rv))
              | Geq -> CConst (Bool (lv >= rv))
              | Eq -> CConst (Bool (lv = rv))
              | Neq -> CConst (Bool (lv <> rv))
              | _ -> raise InterpretationError
            end

          | CConst (Bool lv), CConst (Bool rv) -> begin
              match op with
              | Or -> CConst (Bool (lv || rv))
              | And -> CConst (Bool (lv && rv))
              | Eq -> CConst (Bool (lv = rv))
              | Neq -> CConst (Bool (lv <> rv))
              | _ -> raise InterpretationError
            end

          | CRef r, _ ->
              if op = SetRef then
                (* references cannot change type *)
                if equal_types rc !r then begin
                  r := rc;
                  CConst Unit
                end else raise InterpretationError
              else raise InterpretationError

          | _ -> raise InterpretationError
        in step env k' kE r
        in step env k' kE l

    | UnaryOp (op, e) ->
        let k' c =
          k <|
          match c with
          | CConst (Int i) ->
              if op = UMinus then CConst (Int (-i))
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
          | CConst (Bool b) ->
              if b then step env k kE truthy
              else step env k kE falsy
          | _ -> raise InterpretationError
        in step env k' kE cond

    | LetIn (p, e, fn) ->
        let k' c =
          let matched, env' = match_pattern env p c in
          if matched then
            step env' k kE fn
          else raise InterpretationError
        in step env k' kE e

    (* no pattern matching for the 1rst token of recursive definitions *)
    | LetRecIn (id, e, fn) -> begin
        match e with
        | Fun (p', e') ->
            let f = CRec(id, p', e', env) in
            let env' = Env.add id f env in
            step env' k kE fn

        (* ain't recursive, or at least not in the way we allow *)
        | _ ->
            let k' c =
              let env' = Env.add id c env
              in step env' k kE fn
            in step env k' kE e
      end

    | Let (pattern, e) ->
        let k' c =
          let matched, env' = match_pattern env pattern c in
          if matched then
            gk env' c
          else raise InterpretationError
        in step env k' kE e

    | LetRec (id, e) -> begin
        match e with
        | Fun (p, e') ->
            let f = CRec(id, p, e', env) in
            let env' = Env.add id f env in
            gk env' f
        | _ ->
            let k' c =
              let env = Env.add id c env
              in gk env c
            in step env k' kE e
      end

    | Fun (pattern, e) ->
        k <| CClosure (pattern, e, env)

    | Call (e, x) ->
        let k' fc =
          match fc with
          | CClosure (pattern, fn, env') ->
              let k' v = 
                let matched, env' = match_pattern env' pattern v in
                if matched then
                  step env' k kE fn
                else raise InterpretationError
              in step env k' kE x

          | CRec (name, pattern, e, env') ->
              let k' v = 
                let matched, env' = match_pattern (Env.add name fc env') pattern v in
                if matched then
                  step env' k kE e
                else raise InterpretationError
              in step env k' kE x

          | CMetaClosure f ->
              let k' v = 
                k <| f v
              in step env k' kE x

          | _ -> raise InterpretationError
        in step env k' kE e

    | Deref e ->
        let k' v =
          match v with
          | CRef r -> k !r
          | _ -> raise InterpretationError
        in step env k' kE e

    | ArraySet (arr, key, v) ->
        let k' arr = 
          match arr with
          | CArray a ->
              let k' p =
                match p with
                | CConst (Int p) when p >= 0 ->
                  if p >= Array.length a then
                    raise InterpretationError
                  else
                  let k' v =
                    match v with
                    | CConst (Int v) ->
                        a.(p) <- v;
                        k (CConst Unit)
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
                | CConst (Int p) when p >= 0 ->
                  if p >= Array.length a then
                    raise InterpretationError
                  else
                    k <| CConst (Int a.(p))
                | _ -> raise InterpretationError
              in step env k' kE key
          | _ -> raise InterpretationError
        in step env k' kE arr

    | Raise e ->
        step env kE kE e

    | TryWith (l, p, r) ->
        let kE' x = 
          let matched, env' = match_pattern env p x in
          if matched then
            step env' k kE r
        in step env k kE' l

    | Seq (l, r) ->
        let k' lc =
          if lc = CConst Unit then
            step env k kE r
          else raise InterpretationError
        in step env k' kE l

    | Tuple (a, b) ->
        let k' av =
        let k' bv =
          k (CTuple (av, bv))
        in step env k' kE b
        in step env k' kE a

  in let _ = step env k kE e 
  in ()
