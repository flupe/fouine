open Ast
open Print
open Shared
open Bytecode

let indent = "  "

let p inline offset txt =
  if not inline then
    print_string offset;
  print_string txt

let rec string_of_value_type = function
  | CConst c -> begin match c with
    | Int _ -> green "int"
    | Bool _ -> yellow "bool"
    | Unit -> magenta "unit"
    | String _ -> cyan "string"
    | Char _ -> yellow "char"
    end
  | CRef r -> string_of_value_type !r ^ red " ref"
  | CArray _ -> cyan "'a array"
  | CTuple tl ->
      "(" ^ (String.concat " * " <| List.map string_of_value_type tl) ^ ")"
  | CMetaClosure _ -> red "builtin"
  | CClosure _ -> blue "fun"
  | CRec _ -> blue "rec fun"
  | CBClosure _ -> blue "fun"
  | CBRec _ -> blue "rec fun"
  | CConstructor _ -> "<constructor>"

(* string_of_const : Ast.constant -> string *)
let string_of_const = function
  | Int i -> green (string_of_int i)
  | Bool b -> yellow (if b then "true" else "false")
  | Unit -> magenta "()"
  | String s -> cyan ("\"" ^ s ^ "\"")
  | Char c -> yellow ("\'" ^ Char.escaped c ^ "\'")

let rec string_of_value = function
  | CConst c -> string_of_const c
  | CRef r -> Printf.sprintf "{ contents = %s }" (string_of_value !r)
  | CArray vl ->
      "[|" ^ (String.concat "; " (List.map string_of_value (Array.to_list vl))) ^ "|]"
  | CTuple vl ->
      "(" ^ (String.concat ", " (List.map string_of_value vl)) ^ ")"
  (*
  | CConstructor ("(::)", [h; t]) ->
      "[" ^ (string_of_value h) ^ (string_list t) ^ "]" *)
  | CConstructor (name, []) -> name
  | CConstructor (name, vl) ->
      name ^ " (" ^ (String.concat ", " (List.map string_of_value vl)) ^ ")"
  | CMetaClosure _ -> red "<builtin>"
  | CClosure _ | CBClosure _ -> red "<fun>"
  | CRec _ | CBRec _ -> red "<cycle>"

(* 
and string_list = function
  | CConstructor ("(::)", [h; t]) ->
      "; " ^ (string_of_value h) ^ (string_list t)
  | CConstructor ("[]", []) -> ""
  | x -> string_of_value x
*)

let print_constant_with f = function
  | Int k -> f (green <| string_of_int k)
  | Bool b -> f (yellow (if b then "true" else "false"))
  | Unit -> f (magenta "()")
  | String s -> f (cyan ("\"" ^ s ^ "\""))
  | Char c -> f (yellow ("\'" ^ Char.escaped c ^ "\'"))

let print_constant = print_constant_with print_string

let rec string_of_pattern = function
  | PAll -> "_"
  | PConst c -> string_of_const c
  | PField id -> id
  | PTuple pl -> Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_pattern pl))
  | PConstructor (name, []) -> name;
  | PConstructor (name, [x]) -> Printf.sprintf "%s %s" name (string_of_pattern x)
  | PConstructor (name, pl) ->
      Printf.sprintf "%s (%s)" name (String.concat ", " (List.map string_of_pattern pl))
  | PConstraint _ -> ""

let rec print_pattern = function
  | PAll -> print_string "_"
  | PConst c -> print_constant c
  | PField id -> print_string id
  | PTuple pl ->
      print_string "(";
      List.iteri (fun i p ->
        if i <> 0 then print_string ", ";
        print_pattern p) pl;
      print_string ")";
  | PConstructor (name, []) -> print_string name;
  | PConstructor (name, [x]) ->
      print_string (name ^ " ");
      print_pattern x;
  | PConstructor (name, pl) ->
      print_string (name ^ " (");
      List.iteri (fun i p ->
        if i <> 0 then print_string ", ";
        print_pattern p) pl;
      print_string ")";
  | PConstraint _ -> ()


and print_value_aux env i o e =
  if not i then
    print_string o;
  let pr = print_string in
  match e with
  | CConst c -> print_constant c
  | CRef r -> pr "-"
  | CArray a ->
      let rec aux acc = function
        | [x] -> acc ^ (green <| string_of_value x)
        | x :: t -> aux (acc ^ green (string_of_value x) ^ "; ") t
        | _ -> acc
      in let values = aux "" (Array.to_list a)
      in pr <| "[| " ^ values ^ " |]"
  | CTuple vl ->
      p i o "(";
      List.iteri (fun i v ->
        if i <> 0 then pr ", ";
        print_value_aux env true (o ^ indent) v) vl;
      pr ")";
  | CClosure (pattern, e, env) ->
      pr (blue "fun ");
      print_pattern pattern;
      pr " -> ";
      print_aux env true (o ^ indent) e
  | CRec (_, e, _) ->
      print_aux env i o e
  | CConstructor _ as s -> pr (string_of_value s)
  | CMetaClosure _
  | CBClosure _
  | CBRec _ -> pr "-"

and esc env inline offset t =
  match t with
  | Const _
  | Tuple _ ->
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
  | Const c -> 
      print_constant_with (p i o) c

  | Var id ->
      if Env.mem id env then
        match Env.find id env with
        | CRec (name, _, _) -> p i o name
        | x -> print_value_aux env i o <| Env.find id env
      else
        p i o (cyan id)

  | Call (fn, x) -> esc i o fn; print_string " "; esc true o x
  | Raise e -> p i o (red "raise "); esc true o e
  | Seq (l, r) -> esc i o l; print_endline ";"; esc false o r

  | IfThenElse (cond, l, r) ->
      p i o (red "if "); esc true (o ^ indent) cond; p true o (red " then\n");
        esc false (o ^ indent) l;
        print_newline();
      p false o (red "else\n");
        esc false (o ^ indent) r

  | Let (pattern, v, e) ->
      p i o (red "let ");
      print_pattern pattern;
      print_string " = \n";
        print_aux false (o ^ indent) v;
        print_newline();
      p false o (red "in\n");
        print_aux false o e

  | LetRec (id, v, e) ->
      p i o (red "let rec " ^ yellow id ^ " = \n");
        print_aux false (o ^ indent) v;
        print_newline();
      p false o (red "in\n");
        print_aux false o e

  | TryWith (fn, e, fail) ->
      p i o (red "try\n");
        esc false (o ^ indent) fn;
      p false o (red "\nwith " ^ blue "E ");
      print_pattern e;
      p true o " ->\n";
        esc false (o ^ indent) fail

  | Fun (pattern, fn) ->
      p i o (blue "fun ");
      print_pattern pattern;
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

  | Tuple vl ->
      p i o "(";
      List.iteri (fun i v ->
        if i <> 0 then print_string ", ";
        print_aux true (o ^ indent) v) vl;
      print_string ")"

  | Array l ->
      p i o "[|";
      List.iteri (fun i v ->
        if i <> 0 then print_string "; ";
        print_aux true (o ^ indent) v) l;
      print_string "|]"

  | Constructor (name, []) -> p i o name
  | Constructor (name, el) ->
      p i o (name ^ " ");
      esc true (o ^ indent) (Tuple el)

  | Constraint (e, tp) -> print_aux i o e

  | MatchWith (e, matching) ->
      p false o "match ";
      esc true (o ^ indent) e;
      print_string " with";
      List.iter (fun (pat, e) ->
        print_newline ();
        p false (o ^ indent) "| ";
        print_pattern pat;
        print_string " -> ";
        esc true (o ^ indent) e
      ) matching

let print_ast e =
  print_aux Env.empty true "" e;
  print_endline ";;"

let rec string_of_type ?clean:(c = false) t =
  let col f x = if c then x else f x in
  let rec aux enclosed = function
    | TInt -> col green "int"
    | TBool -> col yellow "bool"
    | TUnit -> col magenta "unit"
    | TString -> col cyan "string"
    | TChar -> col yellow "char"
    | TRef t -> Printf.sprintf "%s %s" (aux true t) (col red "ref")
    | TArray t -> Printf.sprintf "%s %s" (aux true t) (col cyan "array")
    | TSum (name, []) -> name
    | TSum (name, tl) ->
        let param = match tl with
        | [x] -> aux true x
        | _ -> "(" ^ (String.concat ", " (List.map (aux false) tl)) ^ ")"
      in
      Printf.sprintf "%s %s" param name
    | TGeneric id -> col cyan ("'" ^ id)
    | TVar { contents = Unbound (id, _) } -> col cyan ("'_" ^ id)
    | TVar { contents = Link t } -> aux enclosed t
    | t -> begin
        Printf.sprintf (if enclosed then "(%s)" else "%s") @@ match t with
        | TArrow (ta, tb) -> Printf.sprintf "%s -> %s" (aux true ta) (aux false tb)
        | TTuple tl -> String.concat " * " (List.map (aux true) tl)
        | _ -> ""
      end
  in aux false t

(* string_of_instruction : instruction -> string *)
let rec string_of_instruction = function
  | BConst c -> red "BConst" ^ " (" ^ string_of_const c ^ ")"
  | BTuple n -> red "BTuple" ^ " (" ^ green (string_of_int n) ^ ")"
  | BArray n -> red "BArray" ^ " (" ^ green (string_of_int n) ^ ")"
  | BConstructor (id, n) -> red "BConstructor" ^ " (" ^ cyan id ^ ", " ^ green (string_of_int n) ^ ")"
  | BArraySet -> red "BArraySet"
  | BArrayRead -> red "BArrayRead"
  | BAccess id -> red "BAccess" ^ " (" ^ cyan id ^ ")"
  | BEncap code -> red "BEncap" ^ " (" ^ (string_of_bytecode code) ^ ")"
  | BTry p -> red "BTry" ^ " (" ^ string_of_pattern p ^ ")"
  | BRaise -> red "BRaise"
  | BClosure (pattern, code) -> red "BClosure" ^ " (pat, " ^ (string_of_bytecode code) ^ ")"
  | BRecClosure (f, p, code) -> red "BRecClosure" ^ " (" ^ yellow f ^ ", " ^ string_of_pattern p ^ ", " ^ (string_of_bytecode code) ^ ")"

  | BLet p -> red "BLet" ^ " (" ^ string_of_pattern p ^ ")"
  | BEndLet -> red "BEndLet"
  | BApply -> red "BApply"
  | BBranch -> red "BBranch"
  | BReturn -> red "BReturn"

(* string_of_bytecode : bytecode -> string *)
and string_of_bytecode = function
  | h :: h' :: q ->
      string_of_instruction h ^ "; " ^ string_of_bytecode (h' :: q)
  | h :: [] ->
      string_of_instruction h
  | [] -> ""

let log (name : string option) (t : Ast.tp) (v : Shared.value) = 
  let prefix =
    match name with
    | Some id -> blue "val " ^ id
    | None -> "-"
  in
  Printf.printf "%s : %s = %s\n" prefix (string_of_type t ~clean:false) (string_of_value v);

  match v with
    | CClosure (pat, e, _) ->
        Fun (pat, e) |> print_ast 
    | CRec (id, e, _) ->
        Fun (PField id, e) |> print_ast 
    | CBClosure (_, b, _) 
    | CBRec (_, _, b, _) ->
        string_of_bytecode b |> print_endline
    | _ -> ()

let log_value (v : Shared.value) =
  Printf.printf "- : %s = %s\n" (string_of_value_type v) (string_of_value v)