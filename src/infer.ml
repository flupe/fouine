open Ast

type id = string
type level = int

(* types supported by the fouine language *)
type tp =
  | TConst of id
  | TGeneric of id (* named quantified type variable *)
  | TList of tp
  | TRef of tp
  | TArray of tp
  | TArrow of tp * tp
  | TTuple of tp list
  | TVar of tvar ref

and tvar =
  | Unbound of id * level
  | Link of tp
  
(* type environment *)
type env = (Ast.identifier * tp) list

let base_env : env = 
  [ "ref", TArrow (TGeneric "a", TRef (TGeneric "a"))
  ; "not", TArrow (TConst "bool", TConst "bool")
  ; "prInt", TArrow (TConst "int", TConst "int")
  ; "prOut", TArrow (TGeneric "a", TConst "unit")
  ; "aMake", TArrow (TConst "int",  TArray (TConst "int"))
  ]

let type_of_binop = function
  | Plus | Minus | Mult | Div | Mod -> TArrow (TConst "int", TArrow (TConst "int", TConst "int"))
  | Or | And -> TArrow (TConst "bool", TArrow (TConst "bool", TConst "bool"))
  | Lt | Gt | Leq | Geq | Eq | Neq -> TArrow (TGeneric "a", TArrow (TGeneric "a", TConst "bool"))
  | SetRef -> TArrow (TRef (TGeneric "a"), TArrow (TGeneric "a", TConst "unit"))

let type_of_unop = function
  | UMinus -> TArrow (TConst "int", TConst "int")

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

let rec string_of_type t =
  let rec aux enclosed = function
    | TConst name -> name
    | TGeneric id | TVar { contents = Unbound (id, _) } -> "'" ^ id
    | TVar { contents = Link t } -> aux enclosed t
    | t -> begin
        Printf.sprintf (if enclosed then "(%s)" else "%s") @@ match t with
        | TList t -> Printf.sprintf "%s list" (aux true t)
        | TRef t -> Printf.sprintf "%s ref" (aux true t)
        | TArray t -> Printf.sprintf "%s array" (aux true t)
        | TArrow (ta, tb) -> Printf.sprintf "%s -> %s" (aux false ta) (aux false tb)
        | TTuple tl -> String.concat " * " (List.map (aux true) tl)
        | _ -> ""
      end
  in aux false t

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
 * throw exception if it does, returns unit otherwise *)
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
  | TConst a, TConst b when a = b -> ()
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

  | _ -> failwith (Printf.sprintf "Cannot unify %s with %s." (string_of_type ta) (string_of_type tb))

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
  | Int _ -> TConst "int"
  | Bool _ -> TConst "bool"
  | Unit -> TConst "unit"

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
  | PAll | PConst _ -> env
  (* TODO: check multiple occurences *)
  | PField id -> (id, generalize level (new_var level)) :: env
  | PTuple pl -> List.fold_left (fun env p -> create_type_pattern env level p) env pl

let rec type_of env expr = 
  reset ();
  let rec infer env level = function
    | Var id -> instanciate level (List.assoc id env)
    | Const c -> type_of_const c
    | Tuple l -> TTuple (List.map (infer env level) l)

    (* TODO: consider binops & unops as functions *)
    | BinaryOp (op, x, y) ->
        let t_x = infer env level x in
        let t_y = infer env level y in
        let t_r = new_var level in
        let t_op = type_of_binop op |> instanciate level in
        unify t_op (TArrow (t_x, TArrow (t_y, t_r)));
        t_r

    | UnaryOp (op, x) ->
        let t_x = infer env level x in
        let t_r = new_var level in
        let t_op = type_of_unop op |> instanciate level in
        unify t_op (TArrow (t_x, t_r));
        t_r

    | LetIn (p, v, e) ->
        let tv = infer env (level + 1) v in
        infer (match_type level env tv p) level e

    | IfThenElse (c, a, b) ->
        let t_c = infer env level c in
        let t_a = infer env level a in
        let t_b = infer env level b in
        unify (TConst "bool") t_c;
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
        let env' = create_type_pattern env level p in
        let t_b = infer env' level b in
        unify t_a t_b;
        t_a

    | Raise t -> new_var level

    | Seq (a, b) ->
        let t_b = infer env level b in
        t_b

    | Deref e ->
        let t_e = infer env level e in
        let t = new_var level in
        unify (TRef t) t_e;
        t

    | ArraySet (a, k, v) ->
        let t_a = infer env level a in
        let t_k = infer env level k in
        let t_v = infer env level v in
        let t = new_var level in
        unify (TConst "int") t_k;
        unify (TArray t) t_a;
        unify t t_v;
        TConst "unit"

    | ArrayRead (a, k) ->
        let t_a = infer env level a in
        let t_k = infer env level k in
        let t = new_var level in
        unify (TConst "int") t_k;
        unify (TArray t) t_a;
        t

    | _ -> TConst "unit"
  in prune (infer env 0 expr)
