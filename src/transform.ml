open Ast
open Shared

let make_fn expr = Fun (PTuple [PField "k"; PField "kE"], expr)
let make_success_fn expr = Fun (PTuple [PField "k"; PAll], expr)
let def_args = Tuple [Var "k"; Var "kE"]
let def_pat = PTuple [PField "k"; PField "kE"]

let rec rem_exceptions = function
  | Tuple vl ->
      let names = List.mapi (fun k _ -> "a" ^ string_of_int k) vl in
      let e = Call (Var "k", Tuple (List.map (fun x -> Var x) names)) in
      make_fn <|
        List.fold_right2 (fun x n e -> Call
          ( rem_exceptions x
          , Tuple
            [ Fun (PField n, e)
            ; Var "kE" ])) vl names e

  | Let (p, x, e) ->
      make_fn <| Call
        ( rem_exceptions x
        , Tuple
            [ Fun (p, Call (rem_exceptions e, def_args))
            ; Var "kE"])

  | LetRec _ -> failwith "not supported"

  | IfThenElse (cond, a, b) ->
      make_fn <| Call
        ( rem_exceptions cond
        , Tuple
            [ Fun
              ( PField "b",
                  IfThenElse
                    ( b
                    , Call (rem_exceptions a, def_args)
                    , Call (rem_exceptions b, def_args)
                    ))
              ; Var "kE"])

  (* should not work when functions are stored in variables *)
  | Fun (p, fn) ->
      make_fn <| Call(Var "k", Fun (PTuple [p; def_pat], Call(rem_exceptions fn, def_args)))

  | Call (f, x) ->
      make_fn <| Call
        ( rem_exceptions x
        , Tuple
            [ Fun (PField "x", Call
                ( rem_exceptions f
                , Tuple
                    [ Fun (PField "f", Call (Var "f", Tuple [Var "x"; def_args]))
                    ; Var "kE"]
                ))
            ; Var "kE"])

  | TryWith (a, p, e) ->
      make_fn <| Call
        ( rem_exceptions a
        , Tuple
          [ Var "k"
          ; Fun (p, Call (rem_exceptions e, def_args))])

  | Raise e ->
      make_fn <|
        Call (rem_exceptions e, Tuple [Var "kE"; Var "kE"])

  | x -> make_success_fn <| Call (Var "k", x)


let rec rem_ref = function
  | _ -> failwith "not supported"
