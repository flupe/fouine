open Ast
open Shared
open Beautify

let meta x = CMetaClosure x

let int_binop op =
  meta @@ function
  | CConst (Int a) -> (meta @@ function
     | CConst (Int b) -> CConst (Int (op a b))
     | _ -> raise TypeError)
  | _ -> raise TypeError

let gen_bool_binop op =
  meta (fun a -> meta (fun b -> CConst (Bool (op a b))))

let bool_binop op =
  meta @@ function
  | CConst (Bool a) -> (meta @@ function
     | CConst (Bool b) -> CConst (Bool (op a b))
     | _ -> raise TypeError)
  | _ -> raise TypeError
 
let base =
  List.fold_left (fun e (id, v) -> Env.add id v e) Env.empty <|
    [ "ref", meta (fun x -> CRef (ref x))
    ; "incr", meta (function
        | CRef ({ contents = CConst (Int i)} as r) -> r := CConst (Int (i + 1)); CConst Unit
        | _ -> raise TypeError)
    ; "decr", meta (function
        | CRef ({ contents = CConst (Int i)} as r) -> r := CConst (Int (i - 1)); CConst Unit
        | _ -> raise TypeError)
    ; "not", meta (function CConst (Bool b) -> CConst (Bool (not b)) | _ -> raise TypeError)
    ; "prInt", meta (function CConst (Int i) as x -> print_endline <| string_of_int i; x | _ -> raise TypeError)
    ; "prOut", meta (fun x -> Beautify.log_value x; CConst Unit)
    ; "aMake", meta (function CConst (Int n) when n >= 0 -> CArray (Array.make n (CConst (Int 0))) | _ -> raise TypeError)

    ; "!", meta (function CRef x -> !x | _ -> raise TypeError)
    ; ":=", meta (function 
        | CRef r -> meta (fun x -> r := x; CConst Unit)
        | _ -> raise TypeError)
    ; "+", int_binop (+)
    ; "-", int_binop (-)
    ; "~-", meta (function CConst (Int x) -> CConst (Int ~-x) | _ -> raise TypeError)
    ; "*", int_binop ( * )
    ; "/", int_binop (/)
    ; "mod", int_binop (mod)
    ; "max", int_binop (max)
    ; "min", int_binop (min)
    ; "<", gen_bool_binop (<)
    ; "<=", gen_bool_binop (<=)
    ; ">", gen_bool_binop (>)
    ; ">=", gen_bool_binop (>=)
    ; "=", gen_bool_binop (=)
    ; "<>", gen_bool_binop (<>)
    ; "&&", bool_binop (&&)
    ; "||", bool_binop (||)
    (* ; "|>", meta (fun x -> meta (function CMetaClosure f -> f x | _ -> raise TypeError))
    ; "@@", meta (function CMetaClosure f -> meta (fun x -> f x) | _ -> raise TypeError) *)
    ; "@", meta (fun a -> meta (fun b ->
        let rec aux = function
          | CConstructor ("[]", _) -> b
          | CConstructor ("(::)", [h; t]) -> CConstructor ("(::)", [h; aux t])
          | _ -> raise TypeError
        in aux a
      ))
    ; "^", meta @@ (function
        | CConst (String a) -> (meta @@ function
            | CConst (String b) -> CConst (String (a ^ b))
            | _ -> raise TypeError)
        | _ -> raise TypeError)
    ; "string_split_on_char", meta @@ (function
        | CConst (Char c) -> (meta @@ function
            | CConst (String s) ->
                List.fold_right (fun x e -> CConstructor ("(::)", [x; e])) (List.map (fun x -> CConst (String x)) (String.split_on_char c s)) (CConstructor ("[]", []))
            | _ -> raise TypeError)
        | _ -> raise TypeError)
    ]
