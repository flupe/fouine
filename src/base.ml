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

let rec to_list = function
  | CConstructor ("[]", _) -> []
  | CConstructor ("(::)", [h; t]) -> h :: (to_list t)
  | _ -> raise TypeError

let rec of_list = function
  | [] -> CConstructor ("[]", [])
  | h :: t -> CConstructor ("(::)", [h; of_list t])

let unwrap_assoc = List.map (function
  | CTuple [CConst (Int i); x] -> (i, x)
  | _ -> raise TypeError)

let wrap_assoc = List.map (function
  | (i, x) -> CTuple [CConst (Int i); x])

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
    ; "print_string", meta (function CConst (String s) -> print_string s; CConst Unit | _ -> raise TypeError)
    ; "print_endline", meta (function CConst (String s) -> print_endline s; CConst Unit | _ -> raise TypeError)

    ; "read", meta (fun a -> meta (fun b ->
        let l = to_list a |> unwrap_assoc in
        match b with
          | CConst (Int i) -> List.assoc i l
          | _ -> raise TypeError))

    ; "empty", meta (function
        | CConst (Unit) -> of_list []
        | _ -> raise TypeError)

    ; "allocate", meta (fun a -> meta (fun x ->
        let l = to_list a |> unwrap_assoc in
        let i = List.length l in
        CTuple [CConst (Int i); (i, x) :: l |> wrap_assoc |> of_list]))

    ; "modify", meta (fun a -> meta (fun b -> meta (fun x ->
        let l = to_list a |> unwrap_assoc in
        match b with
          | CConst (Int i) -> (i, x) :: (List.remove_assoc i l) |> wrap_assoc |> of_list
          | _ -> raise TypeError)))

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
    ]
