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

let rec eval (env : constant Env.t) = function
  | Const c -> CConst c

  | BinaryOp (op, l, r) -> begin
      let lc = eval env l in
      let rc = eval env r in
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
    end

  | UnaryOp (op, e) -> begin
      match eval env e with
      | CConst (Int i) ->
          if op = UMinus then CConst (Int (-i))
          else raise InterpretationError
      | _ -> raise InterpretationError
    end

  | Var id ->
      if Env.mem id env then Env.find id env
      else raise InterpretationError

  | IfThenElse (cond, truthy, falsy) -> begin
      match eval env cond with
      | CConst (Bool b) ->
          eval env (if b then truthy else falsy)
      | _ -> raise InterpretationError
    end

  | LetIn (p, e, fn) ->
      let matched, env' = match_pattern env p (eval env e) in
      if matched then eval env' fn
      else raise InterpretationError

  (* no pattern matching for the 1rst token of recursive definitions *)
  | LetRecIn (id, e, fn) -> begin
      match e with
      | Fun (p', e') ->
          let f = CRec(id, p', e', env) in
          eval (Env.add id f env) fn

      (* ain't recursive, or at least not in the way we allow *)
      | _ -> eval (Env.add id (eval env e) env) fn
    end

  (* TODO: see how to handle persistance *)
  | Let (pattern, e) ->
      let c = eval env e in
      let matched, env' = match_pattern env pattern c in
      if matched then c
      else raise InterpretationError

  | LetRec (id, e) -> begin
      match e with
      | Fun (p, e') ->
          let f = CRec(id, p, e', env) in
          (* gk (Env.add id f env) f *)
          f
      | _ ->
          let c = eval env e in
          let env = Env.add id c env in
          (* in gk env c *)
          c
    end

  | Fun (pattern, e) -> CClosure (pattern, e, env)

  | Call (e, x) -> begin
      let fc = eval env e in  
      match fc with
      | CClosure (pattern, fn, env') ->
          let matched, env' = match_pattern env' pattern (eval env x) in
          if matched then eval env' fn
          else raise InterpretationError

      | CRec (name, pattern, e, env') ->
          let matched, env' = match_pattern (Env.add name fc env') pattern (eval env x) in
          if matched then eval env' e
          else raise InterpretationError

      | CMetaClosure f -> f (eval env x)

      | _ -> raise InterpretationError
    end

  | Deref e -> begin
      match eval env e with
      | CRef r -> !r
      | _ -> raise InterpretationError
    end

  | ArraySet (arr, key, v) -> begin
      match eval env arr with
      | CArray a -> begin
          match eval env key with
          | CConst (Int p) when p >= 0 ->
              (* out of bounds *)
              if p >= Array.length a then raise InterpretationError
              else begin
                match eval env v with
                | CConst (Int v) -> a.(p) <- v; CConst Unit
                | _ -> raise InterpretationError
              end
          | _ -> raise InterpretationError
        end
      | _ -> raise InterpretationError
    end

  | ArrayRead (arr, key) -> begin
      match eval env arr with
      | CArray a -> begin
          match eval env key with
          | CConst (Int p) when p >= 0 ->
              (* out of bounds *)
              if p >= Array.length a then raise InterpretationError
              else CConst (Int a.(p))
          | _ -> raise InterpretationError
        end
      | _ -> raise InterpretationError
    end

  | TryWith _
  | Raise _ -> failwith "Exceptions not supported"

  | Seq (l, r) ->
      let lc = eval env l in
      if lc = CConst Unit then eval env r
      else raise InterpretationError

  | Tuple vl -> CTuple (List.map (eval env) vl)
