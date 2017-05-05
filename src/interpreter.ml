open Ast
open Print
open Shared

type value = Shared.value
let env = ref Base.base

let match_pattern env (a : pattern) (b : value) =
  Env.fold Env.add (Shared.match_pattern a b) env

let bind id v = 
  env := Env.add id v !env

let eval k kE e : unit =
  let rec step env k kE = function
    | Empty -> k <| CList []
    | Const c -> k <| CConst c

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

    | Let (p, e, fn) ->
        let k' c =
          let env' = match_pattern env p c in
            step env' k kE fn
        in step env k' kE e

    (* no pattern matching for the 1rst token of recursive definitions *)
    | LetRec (id, e, fn) -> begin
        let f = CRec(id, e, env) in
        let env' = Env.add id f env in
        step env' k kE fn
      end

    | Fun (pattern, e) ->
        k <| CClosure (pattern, e, env)

    | Call (e, x) ->
        let k' fc =
          match fc with
          | CClosure (pattern, fn, env') ->
              let k' v = 
                let env' = match_pattern env' pattern v in step env' k kE fn
              in step env k' kE x

          | CRec (name, fn, env') -> 
              let k' = function
                | CClosure (p, fn, env) ->
                    let k' v = 
                      let env = Env.add name fc env' in
                      let env' = match_pattern env p v in step env' k kE fn
                    in step env k' kE x
                | _ -> raise InterpretationError
              in step env k' kE fn

          | CMetaClosure f ->
              let k' v = 
                k <| f v
              in step env k' kE x

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

    | Cons (a, b) ->
        let k' a =
          let k' = function
            | CList l -> begin match l with
                | [] -> k <| CList [a]
                | x :: _ as t ->
                    if Shared.equal_types a x then
                      k <| CList (a :: t)
                    else raise InterpretationError
              end
                | _ -> raise InterpretationError
          in step env k' kE b
        in step env k' kE a

    | Raise e ->
        step env kE kE e

    | TryWith (l, p, r) ->
        let kE' x = 
          let env' = match_pattern env p x in step env' k kE r
        in step env k kE' l

    | Seq (l, r) ->
        let k' lc =
          if lc = CConst Unit then
            step env k kE r
          else raise InterpretationError
        in step env k' kE l

    | Tuple vl ->
        let k' vl =
          k <| CTuple vl
        in eval_list env k' kE vl

  and eval_list env k kE = function
    | h :: t ->
        let k' v =
        let k' vt =
          k <| v :: vt
        in eval_list env k' kE t
        in step env k' kE h
    | _ -> k []

  in let _ = step !env k kE e 
  in ()
