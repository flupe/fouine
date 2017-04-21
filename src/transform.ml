open Ast
open Shared

let make_fn expr = Fun (PTuple [PField "k"; PField "kE"], expr)
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


  | BinaryOp (op, a, b) ->
      make_fn <| Call
        ( rem_exceptions a
        , Tuple
            [ Fun (PField "a", Call
                ( rem_exceptions b
                , Tuple
                    [ Fun (PField "b", Call (Var "k", BinaryOp (op, Var "a", Var "b")))
                    ; Var "kE"]
                ))
            ; Var "kE"])

  | UnaryOp (op, a) ->
      make_fn <| Call
        ( rem_exceptions a
        , Tuple
            [ Fun (PField "a", Call (Var "k", UnaryOp (op, Var "a")))
            ; Var "kE"])

  (* here we get rid of the let statement, but we could keep it
   * as we have to for recursive definitions *)
  | LetIn (p, x, e) ->
      make_fn <| Call
        ( rem_exceptions x
        , Tuple
            [ Fun (p, Call (rem_exceptions e, def_args))
            ; Var "kE"])

  | LetRecIn (p, x, e) ->
      make_fn <| Call
        ( rem_exceptions x
        , Tuple
            [ Fun (PField "x",
                LetRecIn (p, Var "x", Call (rem_exceptions e, def_args)))
            ; Var "kE"])

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
      make_fn <| Call(Var "k", Fun (p, Call(rem_exceptions fn, def_args)))

  | Call (f, x) ->
      make_fn <| Call
        ( rem_exceptions x
        , Tuple
            [ Fun (PField "x", Call
                ( rem_exceptions f
                , Tuple
                    [ Fun (PField "f", Call (Var "k", Call (Var "f", Var "x")))
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

  | x -> make_fn <| Call (Var "k", x)

