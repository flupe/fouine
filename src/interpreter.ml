open Expr

exception InterpretationError

(* eval : expr -> constant *)
let eval e =
  let env = Env.empty in

  let rec step env = function
    | Constant c -> c

    | BinaryOp (op, l, r) -> begin
        let lc = step env l in
        let rc = step env r in

        match lc, rc with
        | Int lv, Int rv -> begin
            match op with
            | Plus -> Int (lv + rv)
            | Minus -> Int (lv - rv)
            | Mult -> Int (lv * rv)
            | Lt -> Bool (lv < rv)
            | Gt -> Bool (lv > rv)
            | Leq -> Bool (lv <= rv)
            | Geq -> Bool (lv >= rv)
            | _ -> raise InterpretationError
          end

        | Bool lv, Bool rv -> begin
            match op with
            | Or -> Bool (lv || rv)
            | And -> Bool (lv && rv)
            | _ -> raise InterpretationError
          end

        | Ref r, _ ->
            if op = SetRef then begin
              r := rc;
              Unit
            end
            else raise InterpretationError

        | _ -> raise InterpretationError
      end

    | UnaryOp (op, e) -> begin
        let c = step env e in
        match c with
        | Bool b ->
            if op = Not then Bool (not b)
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
        | Bool b ->
            if b then step env truthy
            else step env falsy
        | _ -> raise InterpretationError
      end

    | Let (id, e, fn) ->
        let c = step env e in
        let env' = Env.add id c env in
        step env' fn

    | Fun (id, e) -> Closure (id, e, env)

    | Call (e, x) -> begin
        let fc = step env e in
        match fc with
        | Closure (id, fn, env) ->
            let v = step env x in
            let env' = Env.add id v env in
            step env' fn

        | _ -> raise InterpretationError
      end

    | MakeRef e ->
        let v = step env e in
        Ref (ref v)

    | Deref e -> begin
        let v = step env e in
        match v with
        | Ref r -> !r
        | _ -> raise InterpretationError
      end

    | Print e -> begin
        let v = step env e in
        match v with
        | Int i ->
            print_int i;
            print_newline ();
            v
        | _ -> raise InterpretationError
      end

    | _ -> Unit
  in

  step env e
