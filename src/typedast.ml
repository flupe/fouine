type _ t
  (* atoms *)
  = Unit : unit t
  | Int : int -> int t
  | Bool : bool -> bool t
  | Ref : 'a t -> ('a ref) t
  | Var : Ast.identifier -> 'a t

  (* binops *)
  | Plus : int t * int t -> int t
  | Minus : int t * int t -> int t
  | Mult : int t * int t -> int t
  | Div : int t * int t -> int t
  | Mod : int t * int t -> int t
  | Eq : 'a t * 'a t -> bool t
  | Neq : 'a t * 'a t -> bool t
  | Gt : int t * int t -> bool t
  | Lt : int t * int t -> bool t
  | Geq : int t * int t -> bool t
  | Leq : int t * int t -> bool t

  | And : bool t * bool t -> bool t
  | Or : bool t * bool t -> bool t

  | SetRef : ('a ref) t * 'a t -> unit t

  | IfThenElse : bool t * 'a t * 'a t -> 'a t
  | Call : ('a -> 'b) t * 'a t -> 'b t
  | Fun : string * 'b t -> ('a -> 'b) t
  | Print : int t -> int t
  | AMake : int t -> int array t
  | Deref : 'a ref t -> 'a t
  | Seq : unit t * 'a t -> 'a t
  | Raise : int t -> 'a t

(* will contain any type constructor *)
type _ types = ..

(* merci peio *)
module type Typ = sig
  type t
  type _ types += Typ : t types
end

type 'a typ = (module Typ with type t = 'a)

(* returns a first class module *)
let create (type s) () : s typ =
  (module struct
    type t = s
    type _ types += Typ : t types
  end : Typ with type t = s)

type packed
  = Pack : 'a typ * 'a t -> packed

type ('a, 'b) eq = Refl : ('a, 'a) eq

let (=~) (type a) (type b) (k : a typ) (p : b typ) : (a, b) eq option =
  let module K = (val k : Typ with type t = a) in
  let module P = (val p : Typ with type t = b) in
  match K.Typ with
  | P.Typ -> Some Refl
  | _ -> None

let rec unpack (type a) (t : a typ) (Pack (tv, v)) : a t =
  match t =~ tv with
  | Some Refl -> v
  | _ -> failwith "ok"

let tunit : unit typ = create ()
let tbool : bool typ = create ()
let tint : int typ = create ()
let tarray : int array typ = create ()
type 'a tref = 'a ref typ

let rec fetch : type a. a typ -> Ast.t -> a t =
  fun t e -> unpack t @@ infer e

and infer (e : Ast.t) : packed =
  match e with
  | Ast.Bool b -> Pack (tbool, Bool b)
  | Ast.Int i -> Pack (tint, Int i)
  | Ast.Unit -> Pack (tunit, Unit)

  | Ast.BinaryOp (op, a, b) -> begin
      match op with
      | Ast.Eq | Ast.Neq -> begin
          let Pack(ta, a) = infer a in
          let Pack(tb, b) = infer b in
          match ta =~ tb with
          | Some Refl -> Pack(tbool, begin
              match op with
              | Ast.Eq -> Eq (a, b)
              | Ast.Neq -> Neq (a, b)
              | _ -> failwith "error"
            end)
          | None -> failwith "we cannot run equality check on distinct types"
        end

      | Ast.Plus | Ast.Minus | Ast.Mult | Ast.Div | Ast.Mod
      | Ast.Lt | Ast.Gt | Ast.Geq | Ast.Leq  -> begin
          let av = fetch tint a in
          let bv = fetch tint b in
          match op with
          | Ast.Plus -> Pack (tint, Plus (av, bv))
          | Ast.Minus -> Pack (tint, Minus (av, bv))
          | Ast.Mult -> Pack (tint, Mult (av, bv))
          | Ast.Div -> Pack (tint, Div (av, bv))
          | Ast.Mod -> Pack (tint, Mod (av, bv))

          | Ast.Gt -> Pack (tbool, Gt (av, bv))
          | Ast.Lt -> Pack (tbool, Lt (av, bv))
          | Ast.Geq -> Pack (tbool, Geq (av, bv))
          | Ast.Leq -> Pack (tbool, Leq (av, bv))

          | _ -> failwith "unsupported"
        end

      | Ast.And -> Pack (tbool, Bool true)
      | Ast.Or -> Pack (tbool, Bool false)

      | _ -> failwith "unsupported"
    end

  | Ast.Var id -> failwith "variables not supported yet"

  | Ast.Seq (a, b) ->
      let a = fetch tunit a in
      let Pack(tb, b) = infer b in
      Pack (tb, Seq(a, b))

  | Ast.IfThenElse (cond, h, l) -> begin
      let cond = fetch tbool cond in
      let Pack (th, h) = infer h in
      let Pack (tl, l) = infer l in
      match th =~ tl with
      | Some Refl -> Pack (th, IfThenElse (cond, h, l))
      | None -> failwith "both branch should be of the same type."
    end

  | Ast.Raise i ->
      let i = fetch tint i in
      Pack (tunit, Raise i)

  | Ast.Print i ->
      let i = fetch tint i in
      Pack (tint, Print i)

  | Ast.AMake n ->
      let n = fetch tint n in
      Pack (tarray, AMake n)

  | _ -> failwith "unsupported"

let rec eval : type a. a t -> a = function
  | Unit -> ()
  | Int i -> i
  | Bool b -> b
  | Ref e -> ref @@ eval e

  | Plus (a, b) -> eval a + eval b
  | Minus (a, b) -> eval a - eval b
  | Mult (a, b) -> eval a * eval b
  | Div (a, b) -> eval a / eval b
  | Mod (a, b) -> eval a mod eval b

  | Eq (a, b) -> eval a = eval b
  | Neq (a, b) -> eval a <> eval b
  | Gt (a, b) -> eval a > eval b
  | Lt (a, b) -> eval a < eval b
  | Geq (a, b) -> eval a >= eval b
  | Leq (a, b) -> eval a <= eval b
  | SetRef (r, x) -> eval r := eval x

  | IfThenElse (b, l, r) -> if eval b then eval l else eval r
  | Call (f, x) -> eval f @@ eval x

  (* pas du tout fonctionnel, je sais pas comment typer les variables *)
  | Fun (x, e) -> fun x -> eval e

  | _ -> failwith "unsupported"
