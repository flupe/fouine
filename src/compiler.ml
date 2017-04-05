open Ast
open Secd

exception UnimplementedError

(* compile : Ast.t -> Secd.bytecode *)
let compile e =
  let rec aux = function
    | Unit   -> [UnitConst]
    | Int n  -> [IntConst n]
    | Bool b -> [BoolConst b]
    | Var x  -> [Access x]

    | BinaryOp (op, a, b) ->
        aux a @
        aux b @
        [BinOp op]

    | UnaryOp (op, a) ->
        aux a @
        [UnOp op]

    | Call (a, b) ->
        aux b @
        aux a @
        [Apply]

    | Seq (a, b) -> 
        aux a @
        aux b

    | IfThenElse (cond, a, b) ->
        aux cond @
        [UnitClosure (aux a)] @
        [UnitClosure (aux b)] @
        [Branch]

    | Let (id, a, b) ->
        aux a @
        [Let id] @
        aux b @
        [EndLet]

    | Fun (id, a) ->
        [Closure (id, (aux a) @ [Return])]

    | Print (a) ->
        aux a @
        [Print]

    | _ ->
        raise UnimplementedError in
	
  aux e