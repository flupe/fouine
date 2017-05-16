type identifier = string
type id = string
type level = int

(* types supported by the fouine language *)
type tp =
  | TInt | TBool | TUnit | TString | TChar
  | TSum of id * tp list
  | TGeneric of id (* named quantified type variable *)
  | TArrow of tp * tp
  | TRef of tp
  | TArray of tp
  | TTuple of tp list
  | TVar of tvar ref

and tvar =
  | Unbound of id * level
  | Link of tp

(* type of fouine expressions specifying fouine types *)
type tp_spec = 
  | SUnbound of id
  | SArrow of tp_spec * tp_spec
  | STuple of tp_spec list
  | SSubtype of id * tp_spec list

type constant =
  | Int  of int
  | Bool of bool
  | String of string
  | Char of char
  | Unit 

type pattern =
  | PAll (* underscore, matches everything *)
  | PConst of constant
  | PField of identifier
  | PTuple  of pattern list
  | PConstraint of pattern * tp_spec
  | PConstructor of identifier * pattern list
 
type t =
  | Var  of identifier
  | Const of constant
  | Tuple of t list
  | Array of t list
  | Constructor of string * t list

  | Let of pattern * t * t
  | LetRec of identifier * t * t

  | IfThenElse of t * t * t
  | Fun of pattern * t
  | Call of t * t
  | TryWith of t * pattern * t
  | MatchWith of t * (pattern * t) list
  | Raise of t
  | Seq of t * t
  | ArraySet of t * t * t
  | ArrayRead of t * t

  | Constraint of t * tp_spec

type constructor = identifier * tp_spec list

type type_decl =
  | Alias of tp_spec
  | Sum of constructor list

type stmt =
  | TypeDef of id list * identifier * type_decl
  | Decl of pattern * t
  | DeclRec of identifier * t
  | Expr of t

type prog = stmt list