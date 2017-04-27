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

let rec eval (renv : constant Env.t ref) expr =
  let rec aux env = function
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

    | LetIn (p, e, fn) ->
        let matched, env' = match_pattern env p (aux env e) in
        if matched then aux env' fn
        else raise InterpretationError

    (* no pattern matching for the 1rst token of recursive definitions *)
    | LetRecIn (id, e, fn) -> begin
        match e with
        | Fun (p', e') ->
            let f = CRec(id, p', e', env) in
            aux (Env.add id f env) fn

        (* ain't recursive, or at least not in the way we allow *)
        | _ -> aux (Env.add id (aux env e) env) fn
      end

    | Let (pattern, e) ->
        let c = aux env e in
        let matched, env' = match_pattern env pattern c in
        if matched then begin
          renv := env';
          c (* actually this expression as no value, but *)
        end
        else raise InterpretationError

    | LetRec (id, e) -> begin
        match e with
        | Fun (p, e') ->
            let f = CRec(id, p, e', env) in
            (* gk (Env.add id f env) f *)
            f
        | _ ->
            let c = aux env e in
            let env = Env.add id c env in
            (* in gk env c *)
            c
      end

    | Fun (pattern, e) -> CClosure (pattern, e, env)

    | Call (e, x) -> begin
        let fc = aux env e in  
        match fc with
        | CClosure (pattern, fn, env') ->
            let matched, env' = match_pattern env' pattern (aux env x) in
            if matched then aux env' fn
            else raise InterpretationError

        | CRec (name, pattern, e, env') ->
            let matched, env' = match_pattern (Env.add name fc env') pattern (aux env x) in
            if matched then aux env' e
            else raise InterpretationError

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
        let lc = aux env l in
        if lc = CConst Unit then aux env r
        else raise InterpretationError

    | Tuple vl -> CTuple (List.map (aux env) vl)

    | TryWith _
    | Raise _ -> failwith "Exceptions not supported"
  in aux !renv expr
