open Ast
open Print

type bytecode =
  instruction list

and instruction
  = BConst of Ast.constant
  | BTuple of int
  | BArraySet
  | BArrayRead
  | BAccess of identifier
  | BEncap of bytecode
  | BTry of pattern
  | BRaise
  | BClosure of pattern * bytecode
  | BRecClosure of identifier * pattern * bytecode
  | BLet of pattern
  | BEndLet
  | BApply
  | BBranch
  | BReturn

(* string_of_const : Ast.constant -> string *)
let string_of_const = function
  | Int i -> green (string_of_int i)
  | Bool b -> yellow (if b then "true" else "false")
  | Unit -> magenta "()"

(* string_of_pattern : Ast.pattern -> string *)
let rec string_of_pattern = function
  | PAll -> "_"
  | PConst c -> string_of_const c
  | PField id -> id
  | PTuple pl ->
      String.concat "" 
        (["("] @
          (List.mapi (fun i p ->
            (if i <> 0 then 
              ", "
            else "") ^ 
            string_of_pattern p) 
          pl) @
        [")"])

(* string_of_instruction : instruction -> string *)
let rec string_of_instruction = function
  | BConst c -> red "BConst" ^ " (" ^ string_of_const c ^ ")"
  | BTuple n -> red "BTuple" ^ " (" ^ green (string_of_int n) ^ ")"
  | BArraySet -> red "BArraySet"
  | BArrayRead -> red "BArrayRead"

  | BAccess id ->
      red "BAccess" ^ " (" ^ cyan id ^ ")"

  | BEncap code ->
      red "BEncap" ^ " (" ^ (string_of_bytecode code) ^ ")"
  
  | BTry p -> red "BTry" ^ " (" ^ string_of_pattern p ^ ")"
  | BRaise -> red "BRaise"

  | BClosure (pattern, code) ->
      red "BClosure" ^ " (pat, " ^ (string_of_bytecode code) ^ ")"

  | BRecClosure (f, p, code) ->
      red "BRecClosure" ^ " (" ^ yellow f ^ ", " ^ string_of_pattern p ^ ", " ^ (string_of_bytecode code) ^ ")"

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