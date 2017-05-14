open Ast
open Shared

let make_fn expr = Fun (PTuple [PField "k"; PField "kE"], expr)
let make_success_fn expr = Fun (PTuple [PField "k"; PAll], expr)
let def_args = Tuple [Var "k"; Var "kE"]
let def_pat = PTuple [PField "k"; PField "kE"]

let count = ref 0

let name () = 
  let n = !count in
  incr count;
  "x" ^ (string_of_int n)

let rec rem_exceptions = function
  | Var _
  | Const _ as x -> make_success_fn <| Call (Var "k", x)

  | Tuple vl ->
      let names = List.mapi (fun k _ -> "a" ^ string_of_int k) vl in
      let e = Call (Var "k", Tuple (List.map (fun x -> Var x) names)) in
      make_fn <|
        List.fold_right2 (fun x n e -> Call
          ( rem_exceptions x
          , Tuple
            [ Fun (PField n, e)
            ; Var "kE" ])) vl names e

  | Array vl ->
      let names = List.mapi (fun k _ -> "a" ^ string_of_int k) vl in
      let e = Call (Var "k", Array (List.map (fun x -> Var x) names)) in
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

  | LetRec (id, x, e) ->
      make_fn <| LetRec (id, Call (rem_exceptions x, Tuple
            [ Fun (PField "x", Var "x")
            ; Var "kE"
            ]), Call (rem_exceptions e, def_args))

  | IfThenElse (cond, a, b) ->
      make_fn <|
        Call (rem_exceptions cond
        , Tuple
            [ Fun
              ( PField "b",
                  IfThenElse
                    ( Var "b"
                    , Call (rem_exceptions a, def_args)
                    , Call (rem_exceptions b, def_args)
                    ))
            ; Var "kE"])

  (* should not work when functions are stored in variables *)
  | Fun (p, fn) ->
      make_fn <| Call(Var "k", Fun (PTuple [p; def_pat], Call (rem_exceptions fn, def_args)))

  | Call (f, x) ->
      let x' = name () in
      let f' = name () in
      make_fn <| Call
        ( rem_exceptions x
        , Tuple
            [ Fun (PField x', Call
                ( rem_exceptions f
                , Tuple
                    [ Fun (PField f', Call (Var f', Tuple [Var x'; def_args]))
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
 
  | Seq (a, b) ->
      make_fn <|
        Call (rem_exceptions a, Tuple
          [ Fun (PField "a", Call (rem_exceptions b, def_args))
          ; Var "kE" ])

  (*
  | Cons (a, b) ->
      make_fn <| Call (rem_exceptions a, Tuple
        [ Fun (PField "x", Call (rem_exceptions b, Tuple
          [ Fun (PField "t", Call (Var "k", Cons (Var "x", Var "t")))
          ; Var "kE"
          ]))
        ; Var "kE"
        ])
        *)

  | x -> make_success_fn <| Call (Var "k", x)

let range n =
  let rec aux i =
    if i = n then 
      []
    else
      i :: (aux (i + 1))
  in aux 0

let foldi f a l = 
  let rec aux i a = function
    | [] -> a
    | h :: q -> aux (i + 1) (f i h a) q
  in aux 0 a l

let rec rem_ref_aux ast = match ast with
  | Var _
  | Const _ -> 
      Fun (PField "_s", Tuple [ast; Var "_s"])

  | Tuple l ->
      let n = List.length l in
      let l' = l |> foldi
        (fun i h q ->
          Let 
            ( PTuple 
                [ PField ("_v" ^ (string_of_int i))
                ; PField ("_s" ^ (string_of_int i))]
            , Call 
                ( rem_ref_aux h
                , Var ("_s" ^ (string_of_int (i + 1))))
            , q))

        ( Tuple 
          [ Tuple (List.map 
              (fun i -> Var ("_v" ^ (string_of_int i)))
              (range n))
          ; Var "_s0"]) in
      
      Fun (PField ("_s" ^ (string_of_int n)), l')

  | Array l ->
      let n = List.length l in
      let l' = l |> foldi
        (fun i h q ->
          Let 
            ( PTuple 
                [ PField ("_v" ^ (string_of_int i))
                ; PField ("_s" ^ (string_of_int i))]
            , Call 
                ( rem_ref_aux h
                , Var ("_s" ^ (string_of_int (i + 1))))
            , q))

        ( Tuple 
          [ Array (List.map 
              (fun i -> Var ("_v" ^ (string_of_int i)))
              (range n))
          ; Var "_s0"]) in
      
      Fun (PField ("_s" ^ (string_of_int n)), l')

  | Let (p, e1, e2) ->
      Fun
        ( PField "_s"
        , Let
            ( PTuple [p; PField "_s'"]
            , Call (rem_ref_aux e1, Var "_s")
            , Call (rem_ref_aux e2, Var "_s'") ))

  (*| LetRec (id, e1, e2) ->
      Fun
        ( PField "_s"
        , LetRec
            ( PTuple [id; PField "_s'"]
            , Call (rem_ref_aux e1, Var "_s")
            , Call (rem_ref_aux e2, Var "_s'") ))*)

  | IfThenElse (cond, e1, e2) ->
      Fun 
        ( PField "_s"
        , Let 
            ( PTuple [PField "_c'"; PField "_s'"]
            , Call (rem_ref_aux cond, Var "_s")
            , IfThenElse 
                ( Var "_c'"
                , Call (rem_ref_aux e1, Var "_s'")
                , Call (rem_ref_aux e2, Var "_s'") )))

  | Fun (p, e) ->
      Fun 
        ( PField "_s"
        , Tuple
            [ Fun (p, rem_ref_aux e)
            ; Var "_s" ])

  | Call (e1, e2) ->
      Fun
        ( PField "_s"
        , Let
            ( PTuple [PField "_v2"; PField "_s2"]
            , Call (rem_ref_aux e2, Var "_s")
            , Let
                ( PTuple [PField "_v1"; PField "_s1"]
                , Call (rem_ref_aux e1, Var "_s2")
                , Call (Call (Var "_v1", Var "_v2"), Var "_s2") )))

  | Seq (e1, e2) ->
      rem_ref_aux (Let (PAll, e1, e2))

  (*| ArraySet of t * t * t
  | ArrayRead of t * t
  | Cons of t * t*)

  | TryWith _ ->
      raise UnsupportedError
  | Raise _ ->
      raise UnsupportedError
  | _ -> ast

let rem_ref ast =
  Let
    ( PField "!"
    , Fun
        ( PField "_s"
        , Fun
            ( PField "_r"
            , Tuple 
                [ Call (Call (Var "read", Var "_s"), Var "_r")
                ; Var "_s"] ))
    , Let
        ( PField ":="
        , Fun
          ( PField "_s"
          , Fun
              ( PField "_r",
                Fun
                  ( PField "_s"
                  , Fun
                      ( PField "_v"
                      , Tuple 
                          [ Const Unit
                          ; Call 
                              (Call (Call (Var "modify", Var "_s"), Var "_r"), Var "_v") ]))))
        , Let 
          ( PField "ref"
          , Fun
              ( PField "_s"
              , Fun
                  ( PField "_v"
                  , Call (Call (Var "allocate", Var "_s"), Var "_v") ))
          , Call 
              ( rem_ref_aux ast
              , Call (Var "empty", Const Unit) ))))
