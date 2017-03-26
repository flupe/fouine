open Print
(* valid identifiers of our lang
 * they must respect a defined format, but this should be handled
 * during tokenisation *)
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
  | SetRef (* good idea? *)

module Env = Map.Make(struct
  type t = identifier
  let compare = Pervasives.compare
end)

(* values that may not be altered by the program *)
type constant
  = Int of int
  | Bool of bool
  | Ref of constant ref
  | Closure of identifier * expr * constant Env.t
  | Unit

and expr
  = Constant of constant
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
  | MakeRef of expr
  | Deref of expr
  | Print of expr


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
  | Constant _
  | Var _ ->
      print_expr e
  | _ ->
      print_string "(";
      print_expr e;
      print_string ")"

and print_expr = function
  | Constant c -> begin
      match c with
      | Int i -> print_string @@ green @@ string_of_int i
      | Unit -> print_string @@ magenta "()"
      | Bool b -> print_string @@ yellow (if b then "true" else "false")
      | Ref r ->
          print_string @@ red "ref ";
          escape (Constant !r);
      | Closure (id, fn, _) ->
          print_string @@ "(" ^ blue "closure " ^ id ^ " -> ";
          escape fn;
          print_string ")";
    end

  | BinaryOp (op, l, r) ->
      escape l;
      print_string @@ string_of_binary_op op;
      escape r

  | UnaryOp (op, r) ->
      print_string @@ string_of_unary_op op;
      escape r

  | Var id -> print_string @@ cyan id

  | IfThenElse (cond, l, r) ->
      print_string @@ red "if ";
      escape cond;
      print_string @@ red " then\n";
      escape l;
      print_string @@ red "\nelse\n";
      escape r

  | Let (id, v, e) ->
      print_string @@ red "let " ^ yellow id ^ " = ";
      print_expr v;
      print_string @@ red " in\n";
      print_expr e

  | LetRec (id, v, e) ->
      print_string @@ red "let rec " ^ yellow id ^ " = ";
      print_expr v;
      print_string @@ red " in\n";
      print_expr e

  | Call (fn, x) ->
      escape fn;
      print_string " ";
      escape x

  | TryWith (fn, e, fail) ->
      print_string @@ red "try\n";
      escape fn;
      print_string @@ red " with " ^ blue "E " ^ e ^ " ->\n";
      escape fail

  | Raise e ->
      print_string @@ red "raise ";
      escape e

  | Fun (id, fn) ->
      print_string @@ blue "fun " ^ yellow id ^ " -> ";
      print_expr fn

  | MakeRef e ->
      print_string @@ red "ref ";
      escape e

  | Deref e ->
      print_string @@ "!";
      escape e

  | Print e ->
      print_string @@ blue "prInt ";
      escape e

let print e =
  print_expr e;
  print_endline ";;"

let print_constant = function
  | Int i ->
      print_endline @@ "- " ^ green "int" ^ " : " ^ string_of_int i

  | Bool b ->
      print_endline @@ "- " ^ yellow "bool" ^ " : " ^ (if b then "true" else "false")

  | Closure (id, e, _) ->
      print_endline @@ "- " ^ blue "fun" ^ " : " ^ yellow id ^ " -> expr"

  | Unit -> print_endline @@ "- " ^ magenta "unit" ^ " : ()"

  | Ref r ->
      print_endline @@ "- " ^ red "ref"
