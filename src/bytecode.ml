open Ast
open Print

type bytecode =
  instruction list

and instruction
  = BConst of Ast.constant
  | BTuple of int
  | BArray of int
  | BConstructor of string * int
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