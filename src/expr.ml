(* valid identifiers of our lang
 * they must respect a defined format, but this should be handled
 * during tokenisation *)
type identifier =
  string

(* values that may not be altered by the program *)
type constant =
  | Bool of bool
  | Int of int
  | Unit

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

