open Ast
open Print

  
(* type environment *)
type env = (Ast.identifier * tp) list

let infix_bool = TArrow (TGeneric "a", TArrow (TGeneric "a", TBool))

let base_env : env = 
  [ "ref", TArrow (TGeneric "a", TRef (TGeneric "a"))
  ; "incr", TArrow (TRef TInt, TUnit)
  ; "decr", TArrow (TRef TInt, TUnit)
  ; "not", TArrow (TBool, TBool)
  ; "prInt", TArrow (TInt, TInt)
  ; "prOut", TArrow (TGeneric "a", TUnit)
  ; "aMake", TArrow (TInt,  TArray TInt)

  ; "+", TArrow (TInt, TArrow (TInt, TInt))
  ; "-", TArrow (TInt, TArrow (TInt, TInt))
  ; "~-", TArrow (TInt, TInt) (* infix negation *)
  ; "*", TArrow (TInt, TArrow (TInt, TInt))
  ; "/", TArrow (TInt, TArrow (TInt, TInt))
  ; "mod", TArrow (TInt, TArrow (TInt, TInt))

  ; "!", TArrow (TRef (TGeneric "a"), TGeneric "a")
  ; ":=", TArrow (TRef (TGeneric "a"), TArrow(TGeneric "a", TUnit))
  ; "<", infix_bool
  ; ">", infix_bool
  ; "<=", infix_bool
  ; ">=", infix_bool
  ; "<>", infix_bool
  ; "=", infix_bool
  ; "&&", TArrow (TBool, TArrow (TBool, TBool))
  ; "||", TArrow (TBool, TArrow (TBool, TBool))

  ; "::", TArrow (TGeneric "a", TArrow (TList (TGeneric "a"), TList (TGeneric "a")))
  ; "@", TArrow (TList (TGeneric "a"), TArrow (TList (TGeneric "a"), TList (TGeneric "a")))
  ; "|>", TArrow (TGeneric "a", TArrow (TArrow (TGeneric "a", TGeneric "b"), TGeneric "b" ))
  ; "@@", TArrow (TArrow (TGeneric "a", TGeneric "b"), TArrow (TGeneric "a", TGeneric "b" ))
  ]

let count = ref 0
let reset () = count := 0 

(* generate a new generic type name
 * go through a, b, ..., z, t1, t2, ... *)
let new_name () =
  let n = !count in
  incr count;
  if n < 26 then String.make 1 (Char.chr (n + 97))
  else "t" ^ string_of_int (n - 25)

(* create a fresh unbound variable at a given level *)
let new_var lvl = TVar (ref (Unbound (new_name (), lvl)))

(* when possible, simplify TVar occurences *)
let rec prune = function
  | TList t -> TList (prune t)
  | TRef t -> TRef (prune t)
  | TArray t -> TArray (prune t)
  | TArrow (ta, tb) -> TArrow (prune ta, prune tb)
  | TTuple tl -> TTuple (List.map prune tl)
  | TVar { contents = Link t } -> prune t
  | t -> t

(* check whether a given type variable appears in some subtype of an unified upper one
 * throw exception if it does, return unit otherwise *)
let rec occurs tvr = function
  | TVar tvr' when tvr == tvr' -> failwith "Recursive occurence of type."
  (* if both type variables are unbound, we grant ownership of the second
   * to the level of the first *)
  | TVar ({ contents = Unbound (id, lvl') } as tvr') ->
      let min_lvl = match !tvr with
        | Unbound (_, lvl) -> min lvl lvl'
        | _ -> lvl'
      in tvr' := Unbound (id, min_lvl)
  | TVar { contents = Link t } -> occurs tvr t
  | TList t -> occurs tvr t
  | TRef t -> occurs tvr t
  | TArray t -> occurs tvr t
  | TArrow (ta, tb) -> occurs tvr ta; occurs tvr tb
  | TTuple tl -> List.iter (occurs tvr) tl
  | _ -> ()

(* when possible, unifies two given types *)
let rec unify ta tb =
  if ta == tb then () (* physical equality *)
  else match ta, tb with
  (* straightforward unification *)
  (* | TConst a, TConst b when a = b -> () *)
  | TArrow (tla, tra), TArrow (tlb, trb) ->
      unify tla tlb;
      unify tra trb
  | TList ta, TList tb -> unify ta tb
  | TArray ta, TArray tb -> unify ta tb
  | TRef ta, TRef tb -> unify ta tb
  | TTuple tla, TTuple tlb -> List.iter2 unify tla tlb
  | TVar { contents = Link ta}, tb
  | ta, TVar { contents = Link tb} -> unify ta tb

  (* unification of unbound type *)
  | TVar ({ contents = Unbound _ } as tvr), t
  | t, TVar ({ contents = Unbound _ } as tvr) ->
      occurs tvr t;
      tvr := Link t

  | _ -> failwith (Printf.sprintf "Cannot unify %s with %s." (Beautify.string_of_type ta ~clean:true) (Beautify.string_of_type tb ~clean:true))

(* quantify a given type at a specific level *)
let rec generalize level = function
  | TList t -> TList (generalize level t)
  | TRef t -> TRef (generalize level t)
  | TArray t -> TArray (generalize level t)
  | TArrow (ta, tb) -> TArrow (generalize level ta, generalize level tb)
  | TTuple tl -> TTuple (List.map (generalize level) tl)
  (* you can only generalize an unbound type in an upper level *)
  | TVar { contents = Unbound (id, level') } when level' > level -> TGeneric id
  | TVar { contents = Link t } -> generalize level t
  | t -> t

(* replace generalized types with new unbound vars *)
let instanciate level t =
  let rec aux mem = function
    | TGeneric id -> begin
        try (List.assoc id mem, mem)
        with Not_found ->
          let tv = new_var level in
          (tv, (id, tv) :: mem)
      end
    | TVar { contents = Link t } -> aux mem t
    | TList t -> let (t, mem) = aux mem t in TList t, mem
    | TRef t -> let (t, mem) = aux mem t in TRef t, mem
    | TArray t -> let (t, mem) = aux mem t in TArray t, mem
    | TArrow (ta, tb) ->
        let (ta, mem) = aux mem ta in
        let (tb, mem) = aux mem tb in
        TArrow (ta, tb), mem
    | TTuple tl ->
        let step (tl, mem) t = let (t, mem) = aux mem t in (t :: tl, mem) in
        let (tl, mem) = List.fold_left step ([], mem) tl in
        TTuple (List.rev tl), mem
    | t -> t, mem
  in fst (aux [] t)

let type_of_const = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Unit -> TUnit

let rec match_type level env tp = function
  | PAll -> env
  | PConst c ->
      unify (type_of_const c) tp;
      env
  (* TODO: check multiple occurences *)
  | PField id -> (id, generalize level tp) :: env
  | PTuple pl ->
      let tl = List.map (fun _ -> new_var level) pl in unify (TTuple tl) tp;
      List.fold_left2 (fun env p t -> match_type level env t p) env pl tl

let rec create_type_pattern env level = function
  | PAll -> new_var level, env
  | PConst c -> type_of_const c, env
  (* TODO: check multiple occurences *)
  | PField id -> let t = new_var level in t, (id, t) :: env
  | PTuple pl -> 
      let step (tl, env) p = let t, env = create_type_pattern env level p in (t :: tl, env) in
      let tl, env = List.fold_left step ([], env) pl in
      TTuple (List.rev tl), env

let rec type_of env expr = 
  reset ();
  let rec infer env level = function
    | Empty -> TList (new_var level)

    | Var id ->
        if List.mem_assoc id env then instanciate level (List.assoc id env)
        else failwith ("Unbound value " ^ id)

    | Const c -> type_of_const c

    | Tuple l -> TTuple (List.map (infer env level) l)

    | Let (p, v, e) ->
        let tv = infer env (level + 1) v in
        infer (match_type level env tv p) level e

    | IfThenElse (c, a, b) ->
        let t_c = infer env level c in
        let t_a = infer env level a in
        let t_b = infer env level b in
        unify TBool t_c;
        unify t_a t_b;
        t_a

    | Fun (p, fn) ->
        let t_var = new_var level in
        let t_ret = infer (match_type level env t_var p) level fn in
        TArrow (t_var, t_ret)

    | Call (fn, x) ->
        let t_fun = infer env level fn in
        let t_var = infer env level x in
        let t_ret = new_var level in
        unify t_fun (TArrow (t_var, t_ret));
        t_ret

    | TryWith (a, p, b) ->
        let t_a = infer env level a in
        let t_p = new_var level in
        let t_b = infer (match_type level env t_p p) level b in
        unify t_a t_b;
        t_a

    | Raise t -> new_var level

    | Seq (a, b) ->
        let t_b = infer env level b in
        t_b

    | ArraySet (a, k, v) ->
        let t_a = infer env level a in
        let t_k = infer env level k in
        let t_v = infer env level v in
        let t = new_var level in
        unify TInt t_k;
        unify (TArray t) t_a;
        unify t t_v;
        TUnit

    | ArrayRead (a, k) ->
        let t_a = infer env level a in
        let t_k = infer env level k in
        let t = new_var level in
        unify TInt t_k;
        unify (TArray t) t_a;
        t

    | _ -> TUnit
  in prune (infer env 0 expr)
