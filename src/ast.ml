open Print

type identifier =
  string

type unary_op
  = Not

type binary_op
  = Plus
  | Minus
  | Mult
  | Div
  | Mod
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
  | Let of identifier * t
  | LetRec of identifier * t
  | LetIn of identifier * t * t
  | LetRecIn of identifier * t * t
  | Call of t * t
  | Raise of t
  | TryWith of t * identifier * t
  | Fun of identifier * t
  | Deref of t
  | Print of t
  | AMake of t
  | ArraySet of identifier * t * t
  | ArrayRead of identifier * t
  | Seq of t * t

let string_of_binary_op = function
  | Plus -> " + "
  | Minus -> " - "
  | Mult -> " * "
  | Div -> " / "
  | Mod -> " mod "
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

let indent = "  "

let print inline offset txt =
  if not inline then
    print_string offset;
  print_string txt

let rec escape inline offset e =
  match e with
  | Int _ | Bool _ | Unit
  | Var _ ->
      print_ast inline offset e
  | _ ->
      print inline offset "(";
      print_ast true (offset ^ indent) e;
      print_string ")"

and print_ast inline offset = function
  | Int i -> print inline offset (green @@ string_of_int i)
  | Unit -> print inline offset (magenta "()")
  | Bool b -> print inline offset (yellow (if b then "true" else "false"))

  | Ref r ->
      print inline offset (red "ref ");
      escape true offset r

  | BinaryOp (op, l, r) ->
      escape inline offset l;
      print_string (string_of_binary_op op);
      escape true offset r

  | UnaryOp (op, r) ->
      print inline offset (string_of_unary_op op);
      escape true offset r

  | Var id -> print inline offset (cyan id)

  | IfThenElse (cond, l, r) ->
      print inline offset (red "if ");
      escape true (offset ^ indent) cond;
      print true offset (red " then\n");
      escape false (offset ^ indent) l;
      print_newline ();
      print false offset (red "else\n");
      escape false (offset ^ indent) r;

  | LetIn (id, v, e) ->
      print inline offset (red "let " ^ yellow id ^ " =\n");
      print_ast false (offset ^ indent) v;
      print_newline ();
      print false offset (red "in\n");
      print_ast false offset e

  | LetRecIn (id, v, e) ->
      print inline offset (red "let rec " ^ yellow id ^ " =\n");
      print_ast false (offset ^ indent) v;
      print_newline ();
      print false offset (red "in\n");
      print_ast false offset e

  | Let (id, v) ->
      print inline offset (red "let " ^ yellow id ^ " =\n");
      print_ast false (offset ^ indent) v;

  | LetRec (id, v) ->
      print inline offset (red "let rec " ^ yellow id ^ " =\n");
      print_ast false (offset ^ indent) v;

  | Call (fn, x) ->
      escape inline offset fn;
      print_string " ";
      escape true offset x

  | TryWith (fn, e, fail) ->
      print inline offset (red "try\n");
      escape false (offset ^ indent) fn;
      print_newline ();
      print false offset (red "with " ^ blue "E " ^ e ^ " ->\n");
      escape false (offset ^ indent) fail

  | Raise e ->
      print inline offset (red "raise ");
      escape true offset e

  | Fun (id, fn) ->
      print inline offset (blue "fun " ^ yellow id ^ " ->\n");
      print_ast false (offset ^ indent) fn

  | Deref e ->
      print inline offset ("!");
      escape true (offset ^ indent) e

  | Print e ->
      print inline offset (blue "prInt ");
      escape true (offset ^ indent) e

  | AMake e ->
      print inline offset (blue "aMake ");
      escape true (offset ^ indent) e

  | ArraySet (id, key, v) ->
      print inline offset (id ^ ".(");
      escape true (offset ^ indent) key;
      print_string ") <- ";
      escape true (offset ^ indent) v

  | ArrayRead (id, key) ->
      print inline offset (id ^ ".(");
      escape true (offset ^ indent) key;
      print_string ")"

  | Seq (l, r) ->
      escape inline offset l;
      print_endline ";";
      escape false offset r

let print e =
  print_ast true "" e;
  print_endline ";;"
