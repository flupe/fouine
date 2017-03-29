open Print

type identifier =
  string

type unary_op
  = Not

type binary_op
  = Plus
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
  | SetRef

type t
  = Unit
  | Int of int
  | Bool of bool
  | Ref of t
  | BinaryOp of binary_op * t * t
  | UnaryOp of unary_op * t
  | Var of identifier
  | IfThenElse of t * t * t
  | Let of identifier * t * t
  | LetRec of identifier * t * t
  | Call of t * t
  | Raise of t
  | TryWith of t * identifier * t
  | Fun of identifier * t
  | Deref of t
  | Print of t
  | Seq of t * t

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
  | Int _ | Bool _ | Unit
  | Var _ ->
      print_ast e
  | _ ->
      print_string "(";
      print_ast e;
      print_string ")"

and print_ast = function
  | Int i -> print_string @@ green @@ string_of_int i

  | Unit -> print_string (magenta "()")

  | Bool b -> print_string (yellow (if b then "true" else "false"))

  | Ref r ->
      print_string (red "ref ");
      escape r;

  | BinaryOp (op, l, r) ->
      escape l;
      print_string (string_of_binary_op op);
      escape r

  | UnaryOp (op, r) ->
      print_string (string_of_unary_op op);
      escape r

  | Var id -> print_string (cyan id)

  | IfThenElse (cond, l, r) ->
      print_string (red "if ");
      escape cond;
      print_string (red " then\n");
      escape l;
      print_string (red "\nelse\n");
      escape r

  | Let (id, v, e) ->
      print_string (red "let " ^ yellow id ^ " = ");
      print_ast v;
      print_string (red " in\n");
      print_ast e

  | LetRec (id, v, e) ->
      print_string (red "let rec " ^ yellow id ^ " = ");
      print_ast v;
      print_string (red " in\n");
      print_ast e

  | Call (fn, x) ->
      escape fn;
      print_string " ";
      escape x

  | TryWith (fn, e, fail) ->
      print_string (red "try\n");
      escape fn;
      print_string (red " with " ^ blue "E " ^ e ^ " ->\n");
      escape fail

  | Raise e ->
      print_string (red "raise ");
      escape e

  | Fun (id, fn) ->
      print_string (blue "fun " ^ yellow id ^ " -> ");
      print_ast fn

  | Deref e ->
      print_string ("!");
      escape e

  | Print e ->
      print_string (blue "prInt ");
      escape e

  | Seq (l, r) ->
      escape l;
      print_endline ";";
      escape r

let print e =
  print_ast e;
  print_endline ";;"
