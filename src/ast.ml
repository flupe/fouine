type identifier =
  string

type unary_op
  = UMinus

type binary_op
  = Plus | Minus | Mult | Or
  | And  | Lt    | Gt   | Leq
  | Geq  | Eq    | Neq  | SetRef 
  | Div  | Mod

type constant =
  | Int  of int
  | Bool of bool
  | Unit 

type pattern =
  | PAll (* underscore, matches everything *)
  | PConst of constant
  | PField of identifier
  | PPair  of pattern * pattern

type t
  = Var  of identifier
  | Const of constant
  | Tuple of t * t

  | BinaryOp of binary_op * t * t
  | UnaryOp  of unary_op  * t

  | Let of pattern * t
  | LetRec of identifier * t
  | LetIn of pattern * t * t
  | LetRecIn of identifier * t * t

  | IfThenElse of t * t * t
  | Fun of pattern * t
  | Call of t * t
  | TryWith of t * pattern * t
  | Raise of t
  | Seq of t * t
  | Deref of t
  | ArraySet of t * t * t
  | ArrayRead of t * t

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
  | UMinus -> "-"
