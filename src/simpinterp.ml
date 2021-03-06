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

    | MatchWith (e, matching) ->
        let x = aux env e in
        let rec aux' = function
          | (p, r) :: t -> begin
              try aux (match_pattern env p x) r
              with MatchError -> 
                aux' t
            end
          | [] -> raise MatchError
        in aux' matching

    | Fun (pattern, e) -> CClosure (pattern, e, env)

    | Call (e, x) -> begin
        let fc = aux env e in  
        match reduce fc with
        | CClosure (pattern, fn, env') -> 
            let x = aux env x in
            let env' = match_pattern env' pattern x in aux env' fn

        | CRec _ -> print_endline "should not be here"; raise InterpretationError

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
                  a.(p) <- aux env v;
                  CConst Unit
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
                else a.(p)
            | _ -> raise InterpretationError
          end
        | _ -> raise InterpretationError
      end

    | Seq (l, r) ->
        ignore <| aux env l;
        aux env r

    | Tuple vl -> CTuple (List.map (aux env) vl)

    | Constraint (e, _) -> aux env e

    | Array l ->
        CArray (Array.of_list (List.map (aux env) l))

    | Constructor (name, vl) -> begin
        let l = List.map (aux env) vl in
        let params, _ = List.assoc name !Infer.constructors in
        match l, params with
        | [x], [_] -> CConstructor (name, [x])
        | _, [_] -> CConstructor (name, [CTuple l])
        | _ -> CConstructor (name, l)
      end

    | TryWith _
    | Raise _ -> failwith "Exceptions not supported"
  in aux env expr

let meta f = CMetaClosure f
let rec to_list = function
  | CConstructor ("[]", _) -> []
  | CConstructor ("(::)", [h; t]) -> h :: (to_list t)
  | _ -> raise TypeError

let rec of_list = function
  | [] -> CConstructor ("[]", [])
  | h :: t -> CConstructor ("(::)", [h; of_list t])

let unwrap_assoc = List.map (function
  | CTuple [CConst (Int i); x] -> (i, x)
  | _ -> raise TypeError)

let wrap_assoc = List.map (function
  | (i, x) -> CTuple [CConst (Int i); x])


let make_interp debug exceptions references = (module struct
  (* If we use transforms, we need to edit our built-ins to
     change their signatures. *)
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
      Env.mapi (fun name v -> rem (List.assoc name !Infer.env) v ) Base.base
  (* If we use transforms, we need to edit our built-ins to
     change their signatures. *)
    else if references then
      let rec rem t v =
        match t, v with
        | TArrow (_, ty), CMetaClosure f ->
            CMetaClosure (fun x -> CMetaClosure (fun v -> CTuple [rem ty (f x); v]))
        | _, x -> x
      in
      Env.mapi (fun name v -> rem (List.assoc name !Infer.env) v ) Base.base
      |> Env.add "read" (meta (fun a -> meta (fun b ->
           let l = to_list a |> unwrap_assoc in
           match b with
             | CConst (Int i) -> List.assoc i l
             | _ -> raise TypeError)))

      |> Env.add "empty" (meta (function
           | CConst (Unit) -> of_list []
           | _ -> raise TypeError))

      |> Env.add "allocate" (meta (fun a -> meta (fun x ->
           let l = to_list a |> unwrap_assoc in
           let i = List.length l in
           CTuple [CConst (Int i); (i, x) :: l |> wrap_assoc |> of_list])))

      |> Env.add "modify" (meta (fun a -> meta (fun b -> meta (fun x ->
          let l = to_list a |> unwrap_assoc in
          match b with
            | CConst (Int i) -> (i, x) :: (List.remove_assoc i l) |> wrap_assoc |> of_list
            | _ -> raise TypeError))))

    else
      Base.base
  end

  let eval k kE e =
    if debug then begin
      print_endline <| bold "Source AST:";
      Beautify.print_ast e;
      print_newline ();
    end;

    (* if we apply the no_exceptions transform, we add some functions to the environment *)
    if exceptions then begin
      print_endline <| bold "Transformed AST:";

      env := !env
        |> Env.add "k" (CMetaClosure (fun x -> k x; CConst Unit))
        |> Env.add "kE" (CMetaClosure (fun x -> kE x; CConst Unit));
      let e' = Call (Transform.rem_exceptions e, Tuple [Var "k"; Var "kE"]) in
      Beautify.print_ast e';
      print_newline ();
      
      if debug then
        print_endline <| bold "Result:";

      ignore <| eval_expr !env e'
    end

    (* transformation on references *)
    else if references then begin
      print_endline <| bold "Transformed AST:";

      let e' = Transform.rem_ref e in
      Beautify.print_ast e';
      print_newline ();

      k (eval_expr !env e')
    end

  let bind id v = env := Env.add id v !env
end : Shared.Interp)
