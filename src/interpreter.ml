open Ast
open Print
open Shared

exception InterpretationError
exception TypeError

let meta x = CMetaClosure x

let int_binop op =
  meta @@ function
  | CConst (Int a) -> (meta @@ function
     | CConst (Int b) -> CConst (Int (op a b))
     | _ -> raise TypeError)
  | _ -> raise TypeError

let gen_bool_binop op =
  meta (fun a -> meta (fun b -> CConst (Bool (op a b))))

let bool_binop op =
  meta @@ function
  | CConst (Bool a) -> (meta @@ function
     | CConst (Bool b) -> CConst (Bool (op a b))
     | _ -> raise TypeError)
  | _ -> raise TypeError

(* the default environment
 * contains our builtin functions *)
let base =
  [ "ref", meta (fun x -> CRef (ref x))
  ; "incr", meta (function
      | CRef ({ contents = CConst (Int i)} as r) -> r := CConst (Int (i + 1)); CConst Unit
      | _ -> raise TypeError)
  ; "decr", meta (function
      | CRef ({ contents = CConst (Int i)} as r) -> r := CConst (Int (i - 1)); CConst Unit
      | _ -> raise TypeError)
  ; "not", meta (function CConst (Bool b) -> CConst (Bool (not b)) | _ -> raise TypeError)
  ; "prInt", meta (function CConst (Int i) as x -> print_endline <| string_of_int i; x | _ -> raise TypeError)
  ; "prOut", meta (fun x -> Beautify.print_value x; CConst Unit)
  ; "aMake", meta (function CConst (Int n) when n >= 0 -> CArray (Array.make n 0) | _ -> raise TypeError)

  ; "!", meta (function CRef x -> !x | _ -> raise TypeError)
  ; ":=", meta (function 
      | CRef r -> meta (fun x -> if equal_types x !r then (r := x; CConst Unit) else raise TypeError)
      | _ -> raise TypeError)
  ; "+", int_binop (+)
  ; "-", int_binop (-)
  ; "~-", meta (function CConst (Int x) -> CConst (Int ~-x) | _ -> raise TypeError)
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
  ; "|>", meta (fun x -> meta (function CMetaClosure f -> f x | _ -> raise TypeError))
  ; "@@", meta (function CMetaClosure f -> meta (fun x -> f x) | _ -> raise TypeError)
  ; "::", meta (fun x -> meta (function
      | CList []  -> CList [x]
      | CList ((a :: _) as t) ->
          if equal_types a x then CList (x :: t)
          else raise TypeError
      | _ -> raise TypeError))
  ; "@", meta (fun a -> meta (fun b ->
      match a, b with
      | CList ([] as a), CList b
      | CList a, CList ([] as b) -> CList (a @ b)
      | CList ((a :: _) as ta), CList ((b :: _) as tb) ->
          if equal_types a b then CList (ta @ tb)
          else raise TypeError
      | _ -> raise TypeError))
  ] |> List.fold_left (fun e (id, v) -> Env.add id v e) Env.empty

let rec match_pattern env (a : pattern) (b : constant) =
  let rec aux penv a b = match a, b with
  | PAll, _ -> true, env
  | PField id, _ ->
      if Env.mem id penv then
        (* i thought ocaml allowed things like this but no *)
        raise InterpretationError
      else true, Env.add id b penv
  | PConst p, CConst c -> p = c, env
  | PTuple pl, CTuple cl ->
      match_list env pl cl
  | _ -> raise InterpretationError
  in
  let matched, env' = aux Env.empty a b in
  if matched then true, Env.fold Env.add env' env
  else false, env

and match_list env al bl = 
  match al, bl with
  | p :: pt, v :: vt ->
      let matched, env' = match_pattern env p v in
      if matched then match_list env' pt vt
      else false, env
  | [], [] -> true, env
  | _ -> raise InterpretationError

let eval (env : constant Env.t) gk kE e : unit =
  let k = gk env in
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

  in let _ = step env k kE e 
  in ()
