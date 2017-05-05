open Ast
open Print
open Shared

type value = Shared.value

let match_pattern = Interpreter.match_pattern

let rec eval_expr env expr =
  let rec step = function
    | CRec (name, e, env) as c -> aux (Env.add name c env) e
    | x -> x

  and reduce = function
    | CRec _ as c -> reduce (step c)
    | x -> x

  and aux env = function
    | Empty -> CList []
    | Const c -> CConst c

    | Var id ->
        if Env.mem id env then Env.find id env
        else raise InterpretationError

    | IfThenElse (cond, truthy, falsy) -> begin
        match aux env cond with
        | CConst (Bool b) ->
            aux env (if b then truthy else falsy)
        | _ -> raise InterpretationError
      end

    | Let (p, e, fn) ->
        let env' = match_pattern env p (aux env e) in aux env' fn

    (* no pattern matching for the 1rst token of recursive definitions *)
    | LetRec (id, e, fn) -> begin
        let f = step (CRec(id, e, env)) in
        aux (Env.add id f env) fn
      end

    | Fun (pattern, e) -> CClosure (pattern, e, env)

    | Call (e, x) -> begin
        let fc = aux env e in  
        match reduce fc with
        | CClosure (pattern, fn, env') -> 
            let x = aux env x in
            let env' = match_pattern env' pattern x in aux env' fn

        | CRec _ -> print_endline "should not be here"; raise InterpretationError

        (* 
        | CRec (name, e, env') ->
            let env' = Env.add name fc env' in
            match aux env' e in
            aux env' (Call (e, x))
        *)

        | CMetaClosure f -> f (aux env x)

        | _ -> raise InterpretationError
      end

    | ArraySet (arr, key, v) -> begin
        match aux env arr with
        | CArray a -> begin
            match aux env key with
            | CConst (Int p) when p >= 0 ->
                (* out of bounds *)
                if p >= Array.length a then raise InterpretationError
                else begin
                  match aux env v with
                  | CConst (Int v) -> a.(p) <- v; CConst Unit
                  | _ -> raise InterpretationError
                end
            | _ -> raise InterpretationError
          end
        | _ -> raise InterpretationError
      end

    | ArrayRead (arr, key) -> begin
        match aux env arr with
        | CArray a -> begin
            match aux env key with
            | CConst (Int p) when p >= 0 ->
                (* out of bounds *)
                if p >= Array.length a then raise InterpretationError
                else CConst (Int a.(p))
            | _ -> raise InterpretationError
          end
        | _ -> raise InterpretationError
      end

    | Seq (l, r) ->
        ignore <| aux env l;
        aux env r

    | Cons (a, b) ->
        let a = aux env a in
        let b = aux env b in begin
          match b with
          | CList l -> begin match l with
              | [] -> CList [a]
              | x :: _ as t ->
                  if Shared.equal_types a x then CList (a :: t)
                  else raise InterpretationError
            end
          | _ -> raise InterpretationError
        end

    | Tuple vl -> CTuple (List.map (aux env) vl)

    | Constraint (e, _) -> aux env e

    | TryWith _
    | Raise _ -> failwith "Exceptions not supported"
  in aux env expr

let make_interp exceptions references = (module struct
  (* we have to edit our default builtings to take k and kE as arguments *)
  let env = ref begin
    if exceptions then
      let rec rem t v =
        match t, v with
        | TArrow (_, ty), CMetaClosure f ->
            CMetaClosure (function
              | CTuple [x; CTuple [CMetaClosure k; _]] -> k (rem ty (f x))
              | CTuple [x; CTuple [CClosure (p, e, env); _]] ->
                  let env' = match_pattern env p (rem ty (f x)) in eval_expr env' e
              (* we're just going to assume k cannot be recursive *)
              | _ -> raise InterpretationError
            )
        | _, x -> x
      in
      Env.mapi (fun name v -> rem (List.assoc name Infer.base_env) v ) Base.base
    else
      Base.base
  end

  let eval k kE e =
    (* if we apply the no_exceptions transform, we add some functions to the environment *)
    if exceptions then begin
      env := !env
        |> Env.add "k" (CMetaClosure (fun x -> k x; CConst Unit))
        |> Env.add "kE" (CMetaClosure (fun x -> kE x; CConst Unit));
      let e' = Call (Transform.rem_exceptions e, Tuple [Var "k"; Var "kE"]) in
      Beautify.print_ast e';
      ignore <| eval_expr !env e'
    end

  let bind id v = env := Env.add id v !env
end : Shared.Interp)
