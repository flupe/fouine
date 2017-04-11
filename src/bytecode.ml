open Ast
open Print

type bytecode =
  instruction list

and instruction
  = UnitConst
  | IntConst of int
  | BoolConst of bool
  | RefConst
  | ArrayConst
  | Deref
  | ArraySet of identifier
  | ArrayRead of identifier
  | UnOp of unary_op
  | BinOp of binary_op
  | Access of identifier
  | Encap of bytecode
  | Closure of identifier * bytecode
  | RecClosure of identifier * identifier * bytecode
  | Let of identifier
  | EndLet of identifier
  | Apply
  | Branch
  | Print
  | Return

(* string_of_instruction : instruction -> string *)
let rec string_of_instruction = function
  | UnitConst -> red "UnitConst"
  | IntConst i -> red "IntConst" ^ " (" ^ green (string_of_int i) ^ ")"
  | BoolConst b -> 
      if b then
        red "BoolConst" ^ " (" ^ green "true" ^ ")"
      else
        red "BoolConst" ^ " (" ^ green "false" ^ ")"
  | RefConst -> red "RefConst"
  | ArrayConst -> red "ArrayConst"
  | Deref -> red "Deref"

  | ArraySet id -> red "ArraySet" ^ " (" ^ cyan id ^ ")"
  | ArrayRead id -> red "ArrayRead" ^ " (" ^ cyan id ^ ")"

  | UnOp op ->
      red "UnOp" ^ " (" ^ magenta (string_of_unary_op op) ^ ")"

  | BinOp op ->
      red "BinOp" ^ " (" ^ magenta (string_of_binary_op op) ^ ")"

  | Access id ->
      red "Access" ^ " (" ^ cyan id ^ ")"

  | Encap code ->
      red "Encap" ^ " (" ^ (string_of_bytecode code) ^ ")"

  | Closure (id, code) ->
      red "Closure" ^ " (" ^ yellow id ^ ", " ^ (string_of_bytecode code) ^ ")"

  | RecClosure (f, id, code) ->
      red "RecClosure" ^ " (" ^ yellow f ^ ", " ^ yellow id ^ ", " ^ (string_of_bytecode code) ^ ")"

  | Let id -> red "Let" ^ " (" ^ yellow id ^ ")"
  | EndLet id -> red "EndLet" ^ " (" ^ yellow id ^ ")"
  | Apply -> red "Apply"
  | Branch -> red "Branch"
  | Print -> red "Print"
  | Return -> red "Return"

(* string_of_bytecode : bytecode -> string *)
and string_of_bytecode = function
  | h :: h' :: q ->
      string_of_instruction h ^ "; " ^ string_of_bytecode (h' :: q)
  | h :: [] ->
      string_of_instruction h
  | [] -> ""
