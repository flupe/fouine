let (<|) = (@@)

(* fouine types *)
type _ tp =
  | TUnit : unit tp
  | TInt : int tp
  | TBool : bool tp
  | TList : 'a tp -> 'a list tp
  | TRef : 'a tp -> 'a ref tp
  | TPair : 'a tp * 'b tp -> ('a * 'b) tp
  | TArr : 'a tp * 'b tp -> ('a -> 'b) tp

let rec string_of_tp : type a. a tp -> string = function
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "bool"
  | TList t -> Printf.sprintf "(%s list)" (string_of_tp t)
  | TRef t -> Printf.sprintf "(%s ref)" (string_of_tp t)
  | TPair (ta, tb) -> Printf.sprintf "(%s * %s)" (string_of_tp ta) (string_of_tp tb)
  | TArr (ta, tb) -> Printf.sprintf "(%s -> %s)" (string_of_tp ta) (string_of_tp tb)

(* type equality predicate *)
type (_, _) eq = Eq : ('a, 'a) eq

let rec equal_types : type a b. a tp -> b tp -> (a, b) eq option =
  fun a b -> match a, b with
  | TUnit, TUnit -> Some Eq
  | TInt, TInt -> Some Eq
  | TBool, TBool -> Some Eq
  | TRef ta, TRef tb -> begin
      match equal_types ta tb with
      | Some Eq -> Some Eq
      | None -> None
    end
  | TList ta, TList tb -> begin
      match equal_types ta tb with
      | Some Eq -> Some Eq
      | None -> None
    end
  | TPair (lta, rta), TPair (ltb, rtb) -> begin
      match equal_types lta ltb, equal_types rta rtb with
      | Some Eq, Some Eq -> Some Eq
      | _ -> None
    end
  | TArr (lta, rta), TArr (ltb, rtb) -> begin
      match equal_types lta ltb, equal_types rta rtb with
      | Some Eq, Some Eq -> Some Eq
      | _ -> None
    end

(* source terms *)
type _ tm =
  | Unit : unit tm
  | Bool : bool -> bool tm
  | Int : int -> int tm
  (* empty list, with specified type *)
  | Empty : 'a tp -> 'a list tm
  | Cons : 'a tm * 'a list tm -> 'a list tm
  | IfThenElse : bool tm * 'a tm * 'a tm -> 'a tm
  | Pair : 'a tm * 'b tm -> ('a * 'b) tm
  (* even without knowing the value of a variable
   * we assume its type *)
  | Var : 'a tp * string -> 'a tm
  | LetIn : string * 'a tm * 'b tm -> 'b tm
  (* sadly, we have to specify the type of a function *)
  | Fun : ('a -> 'b) tp * ('a -> 'b tm) -> ('a -> 'b) tm
  | App : ('a -> 'b) tm * 'a tm -> 'b tm

module Env = Map.Make (struct
  type t = string
  let compare = Pervasives.compare
end)

type packed = Pack : 'a tp * 'a -> packed
type env = packed Env.t

let rec eval : type a. env -> a tm -> a tp * a = fun env t ->
  match t with
  | Int i -> TInt, i
  | Bool b -> TBool, b
  | Unit -> TUnit, () 
  | Empty t -> TList t, []

  | Cons (x, q) ->
      let t, q = eval env q in
      let _, x = eval env x in
      t, x :: q

  | IfThenElse (c, a, b) ->
      let _, c = eval env c in
      eval env (if c then a else b)

  | Pair (a, b) ->
      let ta, a = eval env a in
      let tb, b = eval env b in
      TPair (ta, tb), (a, b)

  | Var (t, id) ->
      if Env.mem id env then
        let Pack (t', v) = Env.find id env in
        match equal_types t t' with
        | Some Eq -> t, v
        | None -> failwith "Type mismatch"
      else failwith ("Unbound variable " ^ id)

  | LetIn (id, v, e) ->
      let t, v = eval env v in
      eval (Env.add id (Pack (t, v)) env) e

  | Fun (t, f) -> t, fun x -> snd <| eval env (f x)
  | App (f, x) -> 
      let _, x = eval env x in
      let TArr (_, tr), f = eval env f in
      tr, f x

(* not very convenient *)
(* plus : (int -> int -> int) tm *)
let plus =
  Fun (TArr(TInt, TArr(TInt, TInt)),
     fun x -> Fun (TArr(TInt, TInt),
       fun y -> Int (x + y)))

let int_binop = TArr(TInt, TArr(TInt, TInt))
let base_env =
  Env.empty
  |> Env.add "+" (Pack (int_binop, (+)))
  |> Env.add "-" (Pack (int_binop, (-)))
  |> Env.add "*" (Pack (int_binop, ( * )))
  |> Env.add "/" (Pack (int_binop, (/)))
  |> Env.add "mod" (Pack (int_binop, (mod)))

let prog1 = App (App (Var(int_binop, "+"), Int 2), Var (TInt, "x"))
let prog2 = LetIn ("x", Int 3, prog1)

(* target language: beta-normal, mu-long, lambda-terms *)
(* maybe get read of this too? *)
type _ nf =
  | NUnit : unit nf
  | NInt : int -> int nf
  | NBool : bool -> bool nf
  | NPair : 'a nf * 'b nf -> ('a * 'b) nf
  | NLam : ('a y -> 'b nf) -> ('a -> 'b) nf
  | NList : 'a nf list -> 'a list nf
  | NRef : 'a nf ref -> 'a ref nf

and _ at =
  | AApp : ('a -> 'b) at * 'a nf -> 'b at
  | AVar : 'a y -> 'a at

and 'a y

let rec reify : type a. a tp -> a -> a nf =
  fun t v -> match t, v with
  | TUnit, () -> NUnit
  | TBool, b -> NBool b
  | TInt, i -> NInt i
  | TPair (a, b), (va, vb) ->
      NPair (reify a va, reify b vb)
  | TArr (ta, tb), f ->
      NLam (fun x -> reify tb (f (reflect ta (AVar x))))
  | TList t, [] -> NList []
  | TList at, a :: q ->
      let NList l = reify t q in
      NList (reify at a :: l)
  | TRef t, r ->
      NRef (ref (reify t !r))

and reflect : type a. a tp -> a at -> a =
  fun t a -> match t with
  | _ -> failwith "not implemented"

(*
    | TArr (a, b), f ->
        fun x -> reflect a (AVar x) 
          <| fun x -> f x
          <| fun v -> reify b v
          <| fun v -> SRet (k, v)
    | TInt, VInt x -> c (NInt x)
    | TBool, VBool x -> c (NBool x)
    | TUnit, VUnit -> c NUnit
    | TRef t, VRef r ->
        reify t !r <| fun v -> c (NRef (ref v))
*)

(*
and reflect : type a. a tp -> a at -> a md = fun t v ->
  match t, v with
  | TArr (a, b), f ->
      fun c -> c
        <| VFun (fun x k -> reify a x 
        <| fun x -> SBind (f, x, fun v -> reflect b (AVal v) (fun v -> k v)))
  | TBool, b -> fun c -> SIf (b, c (VBool true), c (VBool false))
  | TInt, i -> fun c -> c (VInt i)
  | _ -> failwith "error"

*)


(* 
let binop out op =
  Var(VFun(fun a ->
    fun k -> k <| VFun(fun b ->
      fun k -> k <| out (op a b))))

let v_int x = VInt x
let v_bool x = VBool x
let v_unit () = VUnit

(* builtin terms *)
let plus = binop v_int (+)
let minus = binop v_int (-)
let mult = binop v_int ( * )
let div = binop v_int (/)
let mod_ = binop v_int (mod)

let gt = binop v_bool (>)
let lt = binop v_bool (<)
let geq = binop v_bool (>=)
let leq = binop v_bool (<=)

let and_ = binop v_bool (&&)
let or_ = binop v_bool (||)
let eq = binop v_bool (=)
let neq = binop v_bool (<>)
let setref = binop v_unit (:=)

let rec string_of_nf : type a. a nf -> string = function
  | NUnit -> "()"
  | NBool b -> if b then "true" else "false"
  | NInt i -> string_of_int i
  | NRef r -> Printf.sprintf "ref { %s }" (string_of_nf !r)
  | NLam _ -> "<lambda>"

(* 
let rec eval : type a. a tm -> (a tp, a md) =
  fun e -> match e with
  | Var x -> begin
      match x with
    end
  | Fun f -> c <| VFun (fun x k -> f (eval x) k)
  | App (m, n) ->
      eval m
      <| fun (VFun f) -> eval n
      <| fun n -> f n c
  | If (b, m, n) ->
      eval b
      <| fun (VBool b) -> if b then eval m c else eval n c
  | CC m ->
      eval m
      <| fun (VFun f) -> f (VFun (fun x k -> c x)) c

let nbe : type a. a tp -> a tm -> (a nf -> o) -> o =
  fun t m k ->
    eval m <| fun m -> reify t m k

let id = VFun (fun x k -> k x)
let app = VFun (fun (VFun f) k -> k (VFun (fun x k -> f x (fun v -> k v))))

*)*)
