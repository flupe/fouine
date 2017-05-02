open Ast
open Print

type bytecode =
  instruction list

and instruction
  = BConst of const
  | BArraySet
  | BArrayRead
  | BAccess of identifier
  | BEncap of bytecode
  | BTry of pattern
  | BRaise
  | BClosure of pattern * bytecode
  | BRecClosure of identifier * pattern * bytecode
  | BLet of pattern
  | BEndLet of pattern
  | BApply
  | BBranch
  | BReturn
(*
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
  | Deref -> red "Deref"
  | ArrayConst -> red "ArrayConst"
  | ArraySet -> red "ArraySet"
  | ArrayRead -> red "ArrayRead"

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
*)
