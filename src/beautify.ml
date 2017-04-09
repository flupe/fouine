open Ast
open Print

let indent = "  "

let p inline offset txt =
  if not inline then
    print_string offset;
  print_string txt

let rec esc inline offset t =
  match t with
  | Int _ | Bool _ | Unit | Var _ ->
      print_ast inline offset t
  | _ ->
      p inline offset "(";
      print_ast true (offset ^ indent) t;
      print_string ")"

and print_ast i o = function
  | Int k -> p i o (green @@ string_of_int k)
  | Unit -> p i o (magenta "()")
  | Bool b -> p i o (yellow (if b then "true" else "false"))
  | Ref r -> p i o (red "ref "); esc true o r
  | Var id -> p i o (cyan id)

  | Call (fn, x) -> esc i o fn; print_string " "; esc true o x
  | Raise e -> p i o (red "raise "); esc true o e
  | Deref e -> p i o ("!"); esc true (o ^ indent) e
  | Print e -> p i o (blue "prInt "); esc true (o ^ indent) e
  | AMake e -> p i o (blue "aMake "); esc true (o ^ indent) e
  | Seq (l, r) -> esc i o l; print_endline ";"; esc false o r
  | UnaryOp (op, r) -> p i o (string_of_unary_op op ^ " "); esc true o r

  | BinaryOp (op, l, r) ->
      esc i o l; print_string (" " ^ string_of_binary_op op ^ " "); esc true o r

  | IfThenElse (cond, l, r) ->
      p i o (red "if "); esc true (o ^ indent) cond; p true o (red " then\n");
        esc false (o ^ indent) l;
        print_newline();
      p false o (red "else\n");
        esc false (o ^ indent) r;

  | LetIn (id, v, e) ->
      p i o (red "let " ^ yellow id ^ " = \n");
        print_ast false (o ^ indent) v;
        print_newline();
      p false o (red "in\n");
        print_ast false o e

  | LetRecIn (id, v, e) ->
      p i o (red "let rec " ^ yellow id ^ " =\n");
      print_ast false (o ^ indent) v;
      print_newline();
      p false o (red "in\n");
      print_ast false o e

  | Let (id, v) ->
      p i o (red "let " ^ yellow id ^ " =\n");
      print_ast false (o ^ indent) v;

  | LetRec (id, v) ->
      p i o (red "let rec " ^ yellow id ^ " =\n");
      print_ast false (o ^ indent) v;

  | TryWith (fn, e, fail) ->
      p i o (red "try\n");
        esc false (o ^ indent) fn;
      p false o (red "\nwith " ^ blue "E " ^ e ^ " ->\n");
        esc false (o ^ indent) fail

  | Fun (id, fn) ->
      p i o (blue "fun " ^ yellow id ^ " ->\n");
        print_ast false (o ^ indent) fn

  | ArraySet (id, key, v) ->
      p i o (id ^ ".("); esc true (o ^ indent) key;
      print_string ") <- "; esc true (o ^ indent) v

  | ArrayRead (id, key) ->
      p i o (id ^ ".("); esc true (o ^ indent) key; print_string ")"

let print e =
  print_ast true "" e;
  print_endline ";;"
