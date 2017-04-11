open Ast
open Bytecode

exception UnimplementedError

(** compile : Ast.t -> Bytecode.bytecode
  
  Transforms a Fouine AST into SECD bytecode, i.e. a list of SECD instructions
  to be read by the virtual machine implemented in the `Secd` module. *)
let compile e =
  let rec aux = function
    | Unit   -> [UnitConst]
    | Int n  -> [IntConst n]
    | Bool b -> [BoolConst b]
    | Var x  -> [Access x]

    | Deref a ->
        aux a @
        [Deref]

    | ArraySet (a, k, v) ->
        aux a @
        aux k @
        aux v @
        [ArraySet]
        
    | ArrayRead (a, k) ->
        aux a @
        aux k @
        [ArrayRead]

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
        [Encap (aux a)] @
        [Encap (aux b)] @
        [Branch]

    | LetRecIn (id, Fun (id', a'), b) ->
        [RecClosure (id, id', (aux a') @ [Return])] @
        [Let id] @
        aux b @
        [EndLet id]

    | LetRecIn (id, a, b)
    | LetIn (id, a, b) ->
        aux a @
        [Let id] @
        aux b @
        [EndLet id]

    | LetRec (id, Fun (id', a')) ->
        [RecClosure (id, id', (aux a') @ [Return])] @
        [Let id]

    | LetRec (id, a)
    | Let (id, a) ->
        aux a @
        [Let id]

    | Fun (id, a) ->
        [Closure (id, (aux a) @ [Return])]

    | _ ->
        raise UnimplementedError in
	
  aux e