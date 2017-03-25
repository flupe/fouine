(* valid identifiers of our lang
 * they must respect a defined format, but this should be handled
 * during tokenisation *)
type identifier =
  string

type unary_op =
  | Not

type binary_op =
  | Plus
  | Minus
  | Mult
  | Or
  | And
  | Lt
  | Gt
  | Leq
  | Geq
  | Eq
  | Neq
  | SetRef (* good idea? *)

module Env = Map.Make(struct
  type t = identifier
  let compare = Pervasives.compare
end)

(* values that may not be altered by the program *)
type constant =
  | Int of int
  | Bool of bool
  | Closure of identifier * expr * constant Env.t
  | Unit

and expr =
  | Constant of constant
  | BinaryOp of binary_op * expr * expr
  | UnaryOp of unary_op * expr
  | Var of identifier
  (* anonymous function, the id refers to the name of the only parameter *)
  | IfThenElse of expr * expr * expr
  | Let of identifier * expr * expr
  | LetRec of identifier * expr * expr
  | Call of expr * expr
  | Raise of expr
  | TryWith of expr * identifier * expr
  | Fun of identifier * expr

let string_of_binary_op = function
  | Plus -> " + "
  | Minus -> " - "
  | Mult -> " * "
  | Or -> " || "
  | And -> " && "
  | Lt -> " < "
  | Gt -> " > "
  | Leq -> " <= "
  | Geq -> " >= "
  | Eq -> " = "
  | Neq -> " <> "
  | SetRef -> " := "

let string_of_unary_op = function
  | Not -> "not "

let rec escape e =
  match e with
  | Var _ ->
      print_expr e

  | _ -> 
    print_string "(";
    print_expr e;
    print_string ")"

and print_expr = function
  | Constant c -> begin
      match c with
      | Int i -> print_int i
      | Unit -> print_string "()"
      | Bool b -> print_string @@ if b then "true" else "false"
      | Closure (id, fn, _) ->
          print_string @@ "fun " ^ id ^ " -> ";
          escape fn;
          print_newline ()
    end

  | BinaryOp (op, l, r) ->
      escape l;
      print_string @@ string_of_binary_op op;
      escape r

  | UnaryOp (op, r) ->
      print_string @@ string_of_unary_op op;
      escape r

  | Var id -> print_string id

  | IfThenElse (cond, l, r) ->
      print_string "if ";
      escape cond;
      print_string " then ";
      escape l;
      print_string " else ";
      escape r

  | Let (id, v, e) ->
      print_string @@ "let " ^ id ^ " = ";
      escape v;
      print_string " in ";
      escape e

  | LetRec (id, v, e) ->
      print_string @@ "let rec " ^ id ^ " = ";
      escape v;
      print_string " in ";
      escape e

  | Call (fn, x) ->
      escape fn;
      print_string " ";
      escape x

  | TryWith (fn, e, fail) ->
      print_string @@ "try ";
      escape fn;
      print_string @@ " with E " ^ e ^ " -> ";
      escape fail

  | Raise e ->
      print_string "raise ";
      escape e

  | Fun (id, fn) ->
      print_string @@ "fun " ^ id ^ " -> ";
      escape fn;
      print_newline ()

let print e =
  print_expr e;
  print_endline ";;"
