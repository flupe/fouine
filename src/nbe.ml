let (<|) = (@@)

type 'a k (* continuation variable *)
and 'a v  (* value variable *)
and 'a y  (* variable domain *)

(* shallow embedding
 * language of values in CPS *)
type _ vl =
  | VUnit : unit vl
  | VInt : int -> int vl
  | VBool : bool -> bool vl
  | VRef : 'a vl ref -> 'a ref vl
  | VFun : ('a vl -> 'b md) -> ('a -> 'b) vl

(* continuation monad *)
and 'a md = ('a vl -> o) -> o

and 'a at =
  | AVar of 'a y
  | AVal of 'a v

(* target language: beta-normal, mu-long, lambda-terms *)
and _ nf =
  | NUnit : unit nf
  | NInt : int -> int nf
  | NBool : bool -> bool nf
  | NRef : 'a nf ref -> 'a ref nf
  | NLam : ('a y -> 'b k -> o) -> ('a -> 'b) nf

and o =
  | SRet : 'a k * 'a nf -> o
  | SBind : ('a -> 'b) at * 'a nf * ('b v -> o) -> o
  | SIf : bool at * o * o -> o
  | SEnd : 'a tp * 'a nf -> o

(* source language *)
type _ tm =
  | Var : 'a vl -> 'a tm

  | If : bool tm * 'a tm * 'a tm -> 'a tm
  | CC : (('a -> 'b) -> 'a) tm -> 'a tm

  | Plus : int tm * int tm -> int tm
  | Minus : int tm * int tm -> int tm
  | Mult : int tm * int tm -> int tm
  | Div : int tm * int tm -> int tm
  | Mod : int tm * int tm -> int tm

  | Lt : int tm * int tm -> bool tm
  | Gt : int tm * int tm -> bool tm
  | Leq : int tm * int tm -> bool tm
  | Geq : int tm * int tm -> bool tm

  | And : bool tm * bool tm -> bool tm
  | Or : bool tm * bool tm -> bool tm
  | Not : bool tm -> bool tm

  (* ref instanciation *)
  | Ref : 'a tm -> 'a ref tm
  | SetRef : 'a ref tm * 'a tm -> unit tm
  | Deref : 'a ref tm -> 'a tm

  | Seq : unit tm * 'a tm -> 'a tm

  | Lam : ('a vl -> 'b tm) -> ('a -> 'b) tm
  | App : ('a -> 'b) tm * 'a tm -> 'b tm

(* our language types *)
type _ tp =
  | TUnit : unit tp
  | TInt : int tp
  | TBool : bool tp
  | TRef : 'a tp -> 'a ref tp
  | TArr : 'a tp * 'b tp -> ('a -> 'b) tp

let rec string_of_type : type a. a tp -> string = function
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "boot"
  | TRef t -> Printf.sprintf "(%s ref)" (string_of_type t)
  | TArr (ta, tb) -> Printf.sprintf "(%s -> %s)" (string_of_type ta) (string_of_type tb)

let rec string_of_nf : type a. a nf -> string = function
  | NUnit -> "()"
  | NBool b -> if b then "true" else "false"
  | NInt i -> string_of_int i
  | NRef r -> Printf.sprintf "ref { %s }" (string_of_nf !r)
  | NLam _ -> "\\lambda"

let rec eval : type a. a tm -> a md =
  fun e c -> match e with
  | Var x -> c <| x

  | Plus (a, b) -> eval a
      <| fun (VInt a) -> eval b
      <| fun (VInt b) -> c (VInt (a + b))
  | Minus (a, b) -> eval a
      <| fun (VInt a) -> eval b
      <| fun (VInt b) -> c (VInt (a - b))
  | Mult (a, b) -> eval a
      <| fun (VInt a) -> eval b
      <| fun (VInt b) -> c (VInt (a * b))
  | Div (a, b) -> eval a
      <| fun (VInt a) -> eval b
      <| fun (VInt b) -> c (VInt (a / b))
  | Mod (a, b) -> eval a
      <| fun (VInt a) -> eval b
      <| fun (VInt b) -> c (VInt (a mod b))

  | Lt (a, b) -> eval a
      <| fun (VInt a) -> eval b
      <| fun (VInt b) -> c (VBool (a < b))
  | Gt (a, b) -> eval a
      <| fun (VInt a) -> eval b
      <| fun (VInt b) -> c (VBool (a > b))
  | Leq (a, b) -> eval a
      <| fun (VInt a) -> eval b
      <| fun (VInt b) -> c (VBool (a <= b))
  | Geq (a, b) -> eval a
      <| fun (VInt a) -> eval b
      <| fun (VInt b) -> c (VBool (a >= b))

  (* left evaluation priority *)
  | And (a, b) -> eval a
      <| fun a -> let VBool v = a in if v then eval b c else c a
  | Or (a, b) -> eval a
      <| fun a -> let VBool v = a in if v then c a else eval b c
  | Not a -> eval a
      <| fun (VBool a) -> c (VBool (not a))

  | Ref v -> eval v
      <| fun v -> c (VRef (ref v))
  | SetRef (r, v) -> eval r
      <| fun (VRef r) -> eval v
      <| fun v -> r := v; c VUnit
  | Deref r -> eval r
      <| fun (VRef r) -> c !r

  | Seq (a, b) -> eval a
      <| fun (VUnit) -> eval b c

  | Lam f -> c <| VFun (fun x k -> eval (f x) k)
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
  | _ -> failwith "error"

let nbe t m k =
  eval m <| fun m -> reify t m k

let id = VFun (fun x k -> k x)
let app = VFun (fun (VFun f) k -> k (VFun (fun x k -> f x (fun v -> k v))))
