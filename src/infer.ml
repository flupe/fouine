open Ast
open Print
  
(* type environment *)
type env = (Ast.identifier * tp) list

(* never do that *)
let (@>>) x y = TArrow (x, y)
let (%) id t = TSum (id, [t])
let (%%) id tl = TSum (id, tl)
let (??) x = TGeneric x

let count = ref 0
let reset () = count := 0 

let env : env ref = ref
  [ "ref",   ??"a" @>> TRef ??"a"
  ; "incr",  TRef TInt @>> TUnit
  ; "decr",  TRef TInt @>> TUnit
  ; "not",   TBool @>> TBool
  ; "prInt", TInt @>> TInt
  ; "prOut", ??"a" @>> TUnit
  ; "aMake", TInt @>>  TArray TInt

  ; "+",   TInt @>> TInt @>> TInt
  ; "-",   TInt @>> TInt @>> TInt
  ; "~-",  TInt @>> TInt (* infix negation *)
  ; "*",   TInt @>> TInt @>> TInt
  ; "/",   TInt @>> TInt @>> TInt
  ; "mod", TInt @>> TInt @>> TInt

  ; "!",  TRef ??"a" @>> ??"a"
  ; ":=", TRef ??"a" @>> ??"a" @>> TUnit
  ; "<",  ??"a" @>> ??"a" @>> TBool
  ; ">",  ??"a" @>> ??"a" @>> TBool
  ; "<=", ??"a" @>> ??"a" @>> TBool 
  ; ">=", ??"a" @>> ??"a" @>> TBool
  ; "<>", ??"a" @>> ??"a" @>> TBool
  ; "=",  ??"a" @>> ??"a" @>> TBool
  ; "&&", TBool @>> TBool @>> TBool
  ; "||", TBool @>> TBool @>> TBool

  ; "@",  "list" % ??"a" @>> "list" % ??"a" @>> "list" % ??"a"
  ; "|>", ??"a" @>> (??"a" @>> ??"b") @>> ??"b"
  ; "@@", (??"a" @>> ??"b") @>> ??"a" @>> ??"b"
  ]

let constructors = ref
  [ "(::)", ([??"a"; "list" % ??"a"], "list" % ??"a")
  ; "[]", ([], "list" % ??"a")
  ]

let types = ref
  [ "int", ([], TInt)
  ; "bool", ([], TBool)
  ; "unit", ([], TUnit)
  ; "array", (["a"], TArray ??"a")
  ; "ref", (["a"], TRef ??"a")
  ; "list", (["a"], "list" % ??"a")
  ]

let rec debug_string = function
  | TInt -> "TInt"
  | TBool -> "TBool" 
  | TUnit -> "TUnit"
  | TSum (id, tl) ->
      Printf.sprintf "TSum (\"%s\", [%s])" id (String.concat "; " (List.map debug_string tl))
  | TGeneric id -> "TGeneric \"" ^ id ^ "\""
  | TArrow (ta, tb) ->
      Printf.sprintf "TArrow (%s, %s)" (debug_string ta) (debug_string tb)
  | TRef t ->
      Printf.sprintf "TRef (%s)" (debug_string t)
  | TArray t ->
      Printf.sprintf "TArray (%s)" (debug_string t)
  | TTuple tl ->
      Printf.sprintf "TTuple [%s]" (String.concat "; " (List.map debug_string tl))
  | TVar { contents = Unbound (id, _) } ->
      Printf.sprintf "Unbound \"%s\"" id
  | TVar { contents = Link t } -> debug_string t

(* replace generic types with given association *)
let rec specialize mem = function
  | TGeneric id as t ->
      if List.mem_assoc id mem then
        List.assoc id mem
      else t
  | TArrow (ta, tb) -> TArrow (specialize mem ta, specialize mem tb)
  | TRef t -> TRef (specialize mem t)
  | TArray t -> TArray (specialize mem t)
  | TTuple tl -> TTuple (List.map (specialize mem) tl)
  | TSum (name, tl) -> TSum (name, List.map (specialize mem) tl)
  | t -> t

(* takes a type specification and constructs it *)
let rec build_type = function
  | SUnbound id -> TGeneric id
  | SArrow (ta, tb) -> TArrow (build_type ta, build_type tb)
  | STuple tl -> TTuple (List.map build_type tl)
  | SSubtype (name, params) ->
      if List.mem_assoc name !types then begin
        let (args, tp) = List.assoc name !types in
        let rec iter mem a b = match a, b with
          | id :: args, spec :: params -> iter ((id, build_type spec) :: mem) args params
          | [], [] -> specialize mem tp
          | _ -> 
              failwith (Printf.sprintf "Type %s expects %d argument(s) but %d were given"
                name (List.length args) (List.length params))
        in iter [] args params
      end else failwith ("Unbound type " ^ name)

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
  | TArrow (ta, tb) -> TArrow (prune ta, prune tb)
  | TRef t -> TRef (prune t)
  | TArray t -> TArray (prune t)
  | TTuple tl -> TTuple (List.map prune tl)
  | TSum (name, tl) -> TSum (name, List.map prune tl)
  | TVar { contents = Link t } -> prune t
  | t -> t

(* check whether a given type variable appears in some subtype of an unified upper one
 * throw exception if it does, return unit otherwise *)
let rec occurs tvr = function
  | TVar tvr' when tvr == tvr' -> failwith "Recursive occurence of type."
  | TRef t -> occurs tvr t
  | TArray t -> occurs tvr t
  (* if both type variables are unbound, we grant ownership of the second
   * to the level of the first *)
  | TVar ({ contents = Unbound (id, lvl') } as tvr') ->
      let min_lvl = match !tvr with
        | Unbound (_, lvl) -> min lvl lvl'
        | _ -> lvl'
      in tvr' := Unbound (id, min_lvl)
  | TVar { contents = Link t } -> occurs tvr t
  | TArrow (ta, tb) -> occurs tvr ta; occurs tvr tb
  | TSum (_, tl)
  | TTuple tl -> List.iter (occurs tvr) tl
  | _ -> ()

(* TODO: TConst types shouldn't rely solely on names, since you can rebind type_names while leaving them intact *)
(* when possible, unifies two given types *)
let rec unify ta tb =
  if ta == tb then () (* physical equality *)
  else match ta, tb with
  (* straightforward unification *)
  | TRef ta, TRef tb -> unify ta tb
  | TArray ta, TArray tb -> unify ta tb
  | TSum (a, tla), TSum (b, tlb) ->
      List.iter2 unify tla tlb
  | TArrow (tla, tra), TArrow (tlb, trb) ->
      unify tla tlb;
      unify tra trb
  | TTuple tla, TTuple tlb -> List.iter2 unify tla tlb
  | TVar { contents = Link ta}, tb
  | ta, TVar { contents = Link tb} -> unify ta tb

  (* unification of unbound type *)
  | TVar ({ contents = Unbound _ } as tvr), t
  | t, TVar ({ contents = Unbound _ } as tvr) ->
      occurs tvr t;
      tvr := Link t

  | _ -> failwith (Printf.sprintf "Cannot unify %s with %s." (Beautify.string_of_type ta ~clean:true) (Beautify.string_of_type tb ~clean:true))

(* TODO: same really, find a way to not generalize mutable containers *)
(* quantify a given type at a specific level *)
let rec generalize level = function
  | TArrow (ta, tb) -> TArrow (generalize level ta, generalize level tb)
  | TTuple tl -> TTuple (List.map (generalize level) tl)
  | TSum (name, tl) -> TSum (name, List.map (generalize level) tl)
  (* you can only generalize an unbound type in an upper level *)
  | TVar { contents = Unbound (id, level') } when level' > level -> TGeneric id
  | TVar { contents = Link t } -> generalize level t
  | t -> t

(* replace generalized types with new unbound vars *)
let rec instanciate_aux mem level = function
  | TGeneric id -> begin
      try List.assoc id mem, mem
      with Not_found ->
        let tv = new_var level in
        tv, (id, tv) :: mem
    end
  | TVar { contents = Link t } -> instanciate_aux mem level t
  | TRef t -> let (t, mem) = instanciate_aux mem level t in TRef t, mem
  | TArray t -> let (t, mem) = instanciate_aux mem level t in TArray t, mem
  | TArrow (ta, tb) ->
      let (ta, mem) = instanciate_aux mem level ta in
      let (tb, mem) = instanciate_aux mem level tb in
      TArrow (ta, tb), mem
  | TTuple tl ->
      let step (tl, mem) t = let (t, mem) = instanciate_aux mem level t in (t :: tl, mem) in
      let (tl, mem) = List.fold_left step ([], mem) tl in
      TTuple (List.rev tl), mem
  | TSum (name, tl) ->
      let step (tl, mem) t = let (t, mem) = instanciate_aux mem level t in (t :: tl, mem) in
      let (tl, mem) = List.fold_left step ([], mem) tl in
      TSum (name, List.rev tl), mem
  | t -> t, mem

let instanciate level t = fst (instanciate_aux [] level t)

let instanciate_list mem level tl =
  let step t (acc, mem) =
    let t', mem' = instanciate_aux mem level t in
    (t' :: acc, mem')
  in List.fold_right step tl ([], mem)

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
  | PField id -> (id, tp) :: env

  | PTuple pl ->
      let tl = List.map (fun _ -> new_var level) pl in unify (TTuple tl) tp;
      List.fold_left2 (fun env p t -> match_type level env t p) env pl tl

  | PConstructor (name, pl) ->
      if List.mem_assoc name !constructors then begin
        let params, tr = List.assoc name !constructors in
        let params', mem = instanciate_list [] level params in
        let tr', _ = instanciate_aux mem level tr in
        unify tr' tp;
        List.fold_left2 (match_type level) env params' pl
      end
      else failwith ("Unbound constructor " ^ name)

let rec type_of env expr = 
  reset ();
  let rec infer env level = function
    | Var id -> begin
        try instanciate level (List.assoc id env)
        with _ -> failwith ("Unbound value " ^ id)
      end

    | Const c -> type_of_const c

    | Tuple l -> TTuple (List.map (infer env level) l)

    | Let (p, v, e) ->
        let tv = generalize level (infer env (level + 1) v) in
        infer (match_type level env tv p) level e

    | LetRec (id, v, e) ->
        let t = new_var level in
        let env' = (id, t) :: env in
        unify t (infer env' (level + 1) v);
        infer env' level e

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

    | Constraint (e, spec) ->
        let t = instanciate level (build_type spec) in
        let t' = infer env level e in
        (* 
        print_endline (debug_string t);
        print_endline (debug_string t');
        *)
        unify t t';
        t

    | Array l ->
        let t = new_var level in
        List.iter (fun e -> unify t (infer env level e)) l;
        TArray t

    | Constructor (name, p) ->
        try match p, (List.assoc name !constructors) with
          | [], ([], t) -> instanciate level t
          | [e], ([p], tr) ->
              let t = infer env level e in
              let tr, mem = instanciate_aux [] level tr in
              let tdef, _ = instanciate_aux mem level p in
              unify t tdef;
              tr
          | e, ([p], tr) ->
              let t = infer env level (Tuple e) in
              let tr, mem = instanciate_aux [] level tr in
              let tdef, _ = instanciate_aux mem level p in
              unify t tdef;
              tr
          | e, (def, tr) ->
              let ts = List.map (infer env level) e in
              let tr, mem = instanciate_aux [] level tr in
              let tdef, _ = instanciate_list mem level def in
              if List.length ts = List.length tdef then begin
                List.iter2 unify ts tdef;
                tr
              end else failwith (Printf.sprintf "Constructor %s expects %d argument(s) but %d were given" name (List.length tdef) (List.length ts))
        with Not_found -> failwith ("Unbound constructor " ^ name)

  in prune (infer env 1 expr)

let declare_type name params = function
  | Sum cl ->
      let params' = List.map (fun x -> TGeneric x) params in
      let t = TSum (name, params') in
      types := (name, (params, t)) :: !types;
      List.iter (fun (id, tps) -> constructors := (id, (List.map build_type tps, t)) :: !constructors) cl

  | Alias spec ->
      types := (name, (params, build_type spec)) :: !types

let type_of_stmt = function
  | Decl (p, e) ->
      let t = generalize 0 (type_of !env e) in
      env := match_type 0 !env t p;
      t

  | DeclRec (id, e) ->
      let t = new_var 1 in
      let env' = (id, t) :: !env in
      let t' = type_of env' e in
      unify t t';
      let t = generalize 0 t in
      env := (id, t) :: !env;
      t

  | Expr e ->
      type_of !env e
