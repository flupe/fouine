open Ast
open Print
open Structures

let indent = "  "

let p inline offset txt =
  if not inline then
    print_string offset;
  print_string txt

let rec get_type = function
  | CInt _ -> green "int"
  | CBool _ -> yellow "bool"
  (* we enforce non-cyclic references so it can't loop forever *)
  | CRef r -> get_type !r ^ red " ref"
  | CClosure _ -> blue "fun"
  | CRec _ -> blue "rec fun"
  | CArray _ -> cyan "int array"
  | CUnit -> magenta "unit"

let rec print_constant_aux env i o e =
  if not i then
    print_string o;
  let p = print_string in
  match e with
  | CInt i -> p (green <| string_of_int i)
  | CBool b -> p (if b then "true" else "false")
  | CRef r -> p "-"
  | CClosure (id, e, env) ->
      p ("fun " ^ yellow id ^ " -> ");
      print_aux env true (o ^ indent) e
  | CRec (name, id, _, _) -> p (yellow name ^ " " ^ yellow id ^ " -> ast")
  | CArray a ->
      let rec aux acc = function
        | [x] -> acc ^ (green <| string_of_int x)
        | x :: t -> aux (acc ^ green (string_of_int x) ^ "; ") t
        | _ -> acc
      in let values = aux "" (Array.to_list a)
      in p <| "[| " ^ values ^ " |]"

  | CUnit -> p "()"

and esc env inline offset t =
  match t with
  | Int _ | Bool _ | Unit | Var _ ->
      print_aux env inline offset t
  | _ ->
      p inline offset "(";
      print_aux env true (offset ^ indent) t;
      print_string ")"

and print_aux env i o e = 
  (* lazy me is lazy *)
  let print_aux = print_aux env in
  let esc = esc env in
  match e with
  | Int k -> p i o (green @@ string_of_int k)
  | Unit -> p i o (magenta "()")
  | Bool b -> p i o (yellow (if b then "true" else "false"))
  | Ref r -> p i o (red "ref "); esc true o r
  | Var id ->
      if Env.mem id env then
        print_constant_aux env i o <| Env.find id env
      else
        p i o (cyan id)

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
        print_aux false (o ^ indent) v;
        print_newline();
      p false o (red "in\n");
        print_aux false o e

  | LetRecIn (id, v, e) ->
      p i o (red "let rec " ^ yellow id ^ " =\n");
      print_aux false (o ^ indent) v;
      print_newline();
      p false o (red "in\n");
      print_aux false o e

  | Let (id, v) ->
      p i o (red "let " ^ yellow id ^ " =\n");
      print_aux false (o ^ indent) v;

  | LetRec (id, v) ->
      p i o (red "let rec " ^ yellow id ^ " =\n");
      print_aux false (o ^ indent) v;

  | TryWith (fn, e, fail) ->
      p i o (red "try\n");
        esc false (o ^ indent) fn;
      p false o (red "\nwith " ^ blue "E " ^ e ^ " ->\n");
        esc false (o ^ indent) fail

  | Fun (id, fn) ->
      p i o (blue "fun " ^ yellow id ^ " ->\n");
        print_aux false (o ^ indent) fn

  | ArraySet (arr, key, v) ->
      esc i o arr;
      print_string ".(";
      esc true (o ^ indent) key;
      print_string ") <- ";
      esc true (o ^ indent) v

  | ArrayRead (arr, key) ->
      esc i o arr;
      print_string ".(";
      esc true (o ^ indent) key; print_string ")"

let print_ast e =
  print_aux Env.empty true "" e;
  print_endline ";;"

let print_constant e =
  print_string <| "- : " ^ get_type e ^ " = ";
  print_constant_aux Env.empty true "" e;
  print_newline ()


