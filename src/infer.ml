open Ast

type id = string
type level = int

(* types supported by the fouine language *)
type tp =
  | TConst of id
  | TGeneric of id (* named quantified type variable *)
  | TList of tp
  | TArray of tp
  | TArrow of tp * tp
  | TTuple of tp list
  | TVar of tvar ref

and tvar =
  | Unbound of id * level
  | Link of tp
  
(* type environment *)
type env = (Ast.identifier * tp) list

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
let new_var lvl =
  TVar (ref (Unbound (new_name (), lvl)))

(* TODO: add correct parens *)
let rec string_of_type = function
  | TConst name -> name
  | TList t -> Printf.sprintf "%s list" (string_of_type t)
  | TArray t -> Printf.sprintf "%s array" (string_of_type t)
  | TArrow (ta, tb) -> Printf.sprintf "%s -> %s" (string_of_type ta) (string_of_type tb)
  | TGeneric name -> "'" ^ name
  | TTuple tl -> String.concat " * " (List.map string_of_type tl)
  | TVar { contents = Unbound (id, _) } -> "'" ^ id
  | TVar { contents = Link t } -> string_of_type t

(* when possible, simplify TVar occurences *)
let rec prune = function
  | TList t -> TList (prune t)
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
  | TTuple tla, TTuple tlb -> List.iter2 unify tla tlb

  (* unification of linked type *)
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
    | TArray t -> let (t, mem) = aux mem t in TArray t, mem
    | TArrow (ta, tb) ->
        let (ta, mem) = aux mem ta in
        let (tb, mem) = aux mem tb in
        TArrow (ta, tb), mem
    | TTuple tl ->
        let step (tl, mem) t = let (t, mem) = aux mem t in (t :: tl, mem) in
        let (tl, mem) = List.fold_left step ([], mem) tl in
        TTuple tl, mem
    | t -> t, mem
  in fst (aux [] t)

let type_of_const = function
  | Int _ -> TConst "int"
  | Bool _ -> TConst "bool"
  | Unit -> TConst "unit"

let rec type_of env expr = 
  reset ();
  let rec aux env level = function
    | Var id -> instanciate level (List.assoc id env)
    | Const c -> type_of_const c
    | Tuple l -> TTuple (List.map (aux env level) l)

    (* TODO: unary & binary ops, we should consider them as functions from now on *)
    (* TODO: handle pattern matching. it's not difficult but i'm just tired rn *)

    | LetIn (PField id, v, e) ->
        let tv = aux env (level + 1) v in
        aux ((id, generalize level tv) :: env) level e

    | Fun (PField x, fn) ->
        let t_var = new_var level in
        let t_ret = aux ((x, t_var) :: env) level fn in
        TArrow (t_var, t_ret)

    | Call (fn, x) ->
        let t_fun = aux env level fn in
        let t_var = aux env level x in
        let t_ret = new_var level in
        unify t_fun (TArrow (t_var, t_ret));
        t_ret

    | _ -> TConst "unit"
  in prune (aux env 0 expr)
