open Print

type identifier =
  string

type unary_op
  = Not | UMinus

type binary_op
  = Plus | Minus | Mult | Or
  | And  | Lt    | Gt   | Leq
  | Geq  | Eq    | Neq  | SetRef 
  | Div  | Mod

type t
  = Unit
  | Var  of identifier
  | Int  of int
  | Bool of bool
  | Ref  of t

  | BinaryOp of binary_op * t * t
  | UnaryOp  of unary_op  * t

  | Let of identifier * t
  | LetRec of identifier * t
  | LetIn of identifier * t * t
  | LetRecIn of identifier * t * t

  | IfThenElse of t * t * t
  | Fun of identifier * t
  | Call of t * t
  | Raise of t
  | TryWith of t * identifier * t
  | Seq of t * t
  | Deref of t
  | Print of t
  | AMake of t
  | ArraySet of identifier * t * t
  | ArrayRead of identifier * t

let string_of_binary_op = function
  | Plus   -> "+"
  | Minus  -> "-"
  | Mult   -> "*"
  | Or     -> "||"
  | And    -> "&&"
  | Lt     -> "<"
  | Gt     -> ">"
  | Leq    -> "<="
  | Geq    -> ">="
  | Eq     -> "="
  | Neq    -> "<>"
  | SetRef -> ":="
  | Div -> "/"
  | Mod -> "mod"

let string_of_unary_op = function
  | Not -> "not"
  | UMinus -> "-"

