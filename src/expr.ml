(* valid identifiers of our lang
 * they must respect a defined format, but this should be handled
 * during tokenisation *)
type identifier =
  string

(* values that may not be altered by the program *)
type constant =
  int

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

let string_of_unary_op = function
  | Not -> "not "

type expr =
  | Constant of constant
  | BinaryOp of binary_op * expr * expr
  | UnaryOp of unary_op * expr
  | Var of identifier
  (* anonymous function, the identifier refers to the name of the only parameter *)
  | Fun of identifier * expr
  | IfThenElse of expr * expr * expr
  | Let of identifier * expr * expr
  | LetRec of identifier * expr * expr
  | Call of expr * expr
  | Print of expr

let rec escape e =
  match e with
  | Constant _
  | Var _ ->
      print_expr e
  | _ -> 
    print_string "(";
    print_expr e;
    print_string ")"

and print_expr = function
  | Constant i ->
      print_int i

  | BinaryOp (op, l, r) ->
      escape l;
      print_string @@ string_of_binary_op op;
      escape r

  | UnaryOp (op, r) ->
      print_string @@ string_of_unary_op op;
      escape r

  | Var id -> print_string id

  | Fun (id, fn) ->
      print_string @@ "fun " ^ id ^ " -> ";
      escape fn

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

  | Print fn ->
      print_string "prInt ";
      escape fn

let print e =
  print_expr e;
  print_endline ";;"
