open Ast
open Bytecode

exception UnimplementedError

(** compile : Ast.t -> Bytecode.bytecode
  
  Transforms a Fouine AST into SECD bytecode, i.e. a list of SECD instructions
  to be read by the virtual machine implemented in the `Secd` module. *)
let compile e =
  let rec aux = function
    | Empty   -> []
    | Var x   -> [BAccess x]
    | Const c -> [BConst c]
    | Tuple l -> (* todo *)

    | ArraySet (a, k, v) ->
        aux a @
        aux k @
        aux v @
        [BArraySet]
        
    | ArrayRead (a, k) ->
        aux a @
        aux k @
        [BArrayRead]

    | Call (a, b) ->
        aux b @
        aux a @
        [BApply]

    | Seq (a, b) -> 
        aux a @
        aux b

    | IfThenElse (cond, a, b) ->
        aux cond @
        [BEncap (aux a)] @
        [BEncap (aux b)] @
        [BBranch]

    | LetRec (id, Fun (p, a'), b) ->
        [BRecClosure (id, p, (aux a') @ [Return])] @
        [BLet (PField id)] @
        aux b @
        [BEndLet]

    | Let (p, a, b) ->
        aux a @
        [BLet p] @
        aux b @
        [BEndLet]

    | Fun (p, a) ->
        [BClosure (p, (aux a) @ [BReturn])]

    | TryWith (a, p, b) ->
        [BEncap (aux a)] @
        [BEncap (aux b)] @
        [BTry p]

    | Raise a ->
       aux a @
       [BRaise]

    | _ ->
        raise UnimplementedError in
	
  aux e
