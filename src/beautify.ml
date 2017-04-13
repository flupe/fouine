open Ast
open Print
open Shared

let indent = "  "

let p inline offset txt =
  if not inline then
    print_string offset;
  print_string txt

let rec string_of_type = function
  | CConst c -> begin match c with
    | Int _ -> green "int"
    | Bool _ -> yellow "bool"
    | Unit -> magenta "unit"
    end
  | CRef r -> string_of_type !r ^ red " ref"
  | CClosure _ -> blue "fun"
  | CRec _ -> blue "rec fun"
  | CMetaClosure _ -> red "builtin"
  | CArray _ -> cyan "int array"
  | CTuple cl -> "(" ^ String.concat " * " (List.map string_of_type cl) ^ ")"

let print_constant_aux c = function
  | Int k -> c (green <| string_of_int k)
  | Bool b -> c (yellow (if b then "true" else "false"))
  | Unit -> c (magenta "()")

let print_constant = print_constant_aux print_string

let rec print_pattern_uple i o u =
  p i o "(";
  let rec aux = function
    | [c] -> print_pattern true (o ^ indent) c; ()
    | c :: t ->
        print_pattern true (o ^ indent) c;
        print_string ", "; aux t
    | _ -> ()
  in aux u;
  print_string ")"

and print_pattern i o = function
  | PAll -> p i o "_"
  | PConst c -> print_constant_aux (p i o) c
  | PField id -> p i o id
  | PPair pl -> print_pattern_uple i o pl

let rec print_comma_values env o = function
  | [v] ->
      print_value_aux env true o v;
  | v :: t ->
      print_value_aux env true o v;
      print_string ", ";
      print_comma_values env o t
  | _ -> ()

and print_value_aux env i o e =
  if not i then
    print_string o;
  let pr = print_string in
  match e with
  | CConst c -> print_constant c
  | CRef r -> pr "-"
  | CClosure (pattern, e, env) ->
      pr (blue "fun ");
      print_pattern true (o ^ indent) pattern;
      pr " -> ";
      print_aux env true (o ^ indent) e
  | CRec (name, pattern, e, _) ->
      pr (blue "fun ");
      print_pattern true (o ^ indent) pattern;
      pr " -> ";
      print_aux env true (o ^ indent) e
  | CArray a ->
      let rec aux acc = function
        | [x] -> acc ^ (green <| string_of_int x)
        | x :: t -> aux (acc ^ green (string_of_int x) ^ "; ") t
        | _ -> acc
      in let values = aux "" (Array.to_list a)
      in pr <| "[| " ^ values ^ " |]"
  | CMetaClosure _ -> pr "-"
  | CTuple l ->
      p i o "(";
      print_comma_values env (o ^ indent) l;
      p true o ")"
      

and esc env inline offset t =
  match t with
  | Const _
  | Var _ ->
      print_aux env inline offset t
  | _ ->
      p inline offset "(";
      print_aux env true (offset ^ indent) t;
      print_string ")"

and print_uple env i o u =
  p i o "(";
  let rec aux = function
    | [c] -> print_aux env true (o ^ indent) c
    | c :: t ->
        print_aux env true (o ^ indent) c;
        print_string ", "; aux t
    | _ -> ()
  in aux u;
  print_string ")"


and print_aux env i o e = 
  (* lazy me is lazy *)
  let print_aux = print_aux env in
  let esc = esc env in
  match e with
  | Const c -> 
      print_constant_aux (p i o) c

  | Var id ->
      if Env.mem id env then
        match Env.find id env with
        | CRec (name, _, _, _) -> p i o name
        | x -> print_value_aux env i o <| Env.find id env
      else
        p i o (cyan id)

  | Call (fn, x) -> esc i o fn; print_string " "; esc true o x
  | Raise e -> p i o (red "raise "); esc true o e
  | Deref e -> p i o ("!"); esc true (o ^ indent) e
  | Seq (l, r) -> esc i o l; print_endline ";"; esc false o r
  | UnaryOp (op, r) -> p i o (string_of_unary_op op ^ " "); esc true o r

  | BinaryOp (op, l, r) ->
      esc i o l; print_string (" " ^ string_of_binary_op op ^ " "); esc true o r

  | IfThenElse (cond, l, r) ->
      p i o (red "if "); esc true (o ^ indent) cond; p true o (red " then\n");
        esc false (o ^ indent) l;
        print_newline();
      p false o (red "else\n");
        esc false (o ^ indent) r

  | LetIn (pattern, v, e) ->
      p i o (red "let ");
      print_pattern true (o ^ indent) pattern;
      print_string " = \n";
        print_aux false (o ^ indent) v;
        print_newline();
      p false o (red "in\n");
        print_aux false o e

  | LetRecIn (id, v, e) ->
      p i o (red "let rec " ^ yellow id ^ " = \n");
        print_aux false (o ^ indent) v;
        print_newline();
      p false o (red "in\n");
        print_aux false o e

  | Let (pattern, v) ->
      p i o (red "let ");
      print_pattern true (o ^ indent) pattern;
      print_string " =\n";
      print_aux false (o ^ indent) v

  | LetRec (id, v) ->
      p i o (red "let rec " ^ yellow id ^ " = \n");
        print_aux false (o ^ indent) v

  | TryWith (fn, e, fail) ->
      p i o (red "try\n");
        esc false (o ^ indent) fn;
      p false o (red "\nwith " ^ blue "E ");
      print_pattern true (o ^ indent) e;
      p true o " ->\n";
        esc false (o ^ indent) fail

  | Fun (pattern, fn) ->
      p i o (blue "fun ");
      print_pattern true (o ^ indent) pattern;
      print_string " ->\n";
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

  | Tuple l -> print_uple env i o l

let print_ast e =
  print_aux Env.empty true "" e;
  print_endline ";;"

let print_value e =
  print_string <| "- : " ^ string_of_type e ^ " = ";
  print_value_aux Env.empty true "" e;
  print_newline ()
