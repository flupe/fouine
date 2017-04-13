let (<|) = (@@)

(* fouine types *)
type _ tp =
  | TUnit : unit tp
  | TInt : int tp
  | TBool : bool tp
  | TRef : 'a tp -> 'a ref tp
  | TPair : 'a tp * 'b tp -> ('a * 'b) tp
  | TArr : 'a tp * 'b tp -> ('a -> 'b) tp

let rec string_of_tp : type a. a tp -> string = function
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "boot"
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

(* shallow embedding *)
type _ vl =
  | VUnit : unit vl
  | VInt  : int -> int vl
  | VBool : bool -> bool vl
  | VPair : 'a vl * 'b vl -> ('a * 'b) vl
  | VFun  : ('a vl -> 'b vl) -> ('a -> 'b) vl

(* target language: beta-normal, mu-long, lambda-terms *)
and _ nf =
  | NUnit : unit nf
  | NInt : int -> int nf
  | NBool : bool -> bool nf
  | NPair : 'a nf * 'b nf -> ('a * 'b) nf
  | NLam : ('a -> 'b nf) -> ('a -> 'b) nf

(* source terms *)
type _ tm =
  | Unit : unit tm
  | Bool : bool -> bool tm
  | Int : int -> int tm
  | Pair : 'a tm * 'b tm -> ('a * 'b) tm

  (* even without knowing the value of a variable
   * we assume its type *)
  | Var : 'a tp * string -> 'a tm
  | LetIn : string * 'a tm * 'b tm -> 'b tm

  | Fun : ('a -> 'b) tp * ('a vl -> 'b tm) -> ('a -> 'b) tm
  | App : ('a -> 'b) tm * 'a tm -> 'b tm

module Env = Map.Make (struct
  type t = string
  let compare = Pervasives.compare
end)

type packed = Pack : 'a tp * 'a vl -> packed
type env = packed Env.t

let rec eval : type a. env -> a tm -> a tp * a vl = fun env t ->
  match t with
  | Int i -> TInt, VInt i
  | Bool b -> TBool, VBool b
  | Unit -> TUnit, VUnit
  | Pair (a, b) ->
      let ta, a = eval env a in
      let tb, b = eval env b in
      TPair (ta, tb), VPair (a, b)

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

  | Fun (t, f) -> t, VFun (fun x -> snd <| eval env (f x))
  | App (f, x) -> 
      let _, x = eval env x in
      let TArr (_, tr), VFun f = eval env f in
      tr, (f x)

(* not very convenient (but doable) *)
let plus =
  Fun (TArr(TInt, TArr(TInt, TInt)),
     fun (VInt x) -> Fun (TArr(TInt, TInt),
       fun (VInt y) -> Int (x + y)))

let prog1 = App (App (plus, Int 2), Var (TInt, "x"))
let prog2 = LetIn ("x", Int 3, prog1)

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

let rec reify : type a. a tp -> a vl -> (a nf -> o) -> o =
  fun t v c -> match t, v with
    | TArr (a, b), VFun f -> c
        <| NLam (fun x k -> reflect a (AVar x) 
        <| fun x -> f x
        <| fun v -> reify b v
        <| fun v -> SRet (k, v))
    | TInt, VInt x -> c (NInt x)
    | TBool, VBool x -> c (NBool x)
    | TUnit, VUnit -> c NUnit
    | TRef t, VRef r ->
        reify t !r <| fun v -> c (NRef (ref v))

and reflect : type a. a tp -> a at -> a md = fun t v ->
  match t, v with
  | TArr (a, b), f ->
      fun c -> c
        <| VFun (fun x k -> reify a x 
        <| fun x -> SBind (f, x, fun v -> reflect b (AVal v) (fun v -> k v)))
  | TBool, b -> fun c -> SIf (b, c (VBool true), c (VBool false))
  | TInt, i -> fun c -> c (VInt i)
  | _ -> failwith "error"

let nbe : type a. a tp -> a tm -> (a nf -> o) -> o =
  fun t m k ->
    eval m <| fun m -> reify t m k

let id = VFun (fun x k -> k x)
let app = VFun (fun (VFun f) k -> k (VFun (fun x k -> f x (fun v -> k v))))

*)*)
