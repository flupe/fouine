let (@@) f x = f x;;
let (|>) x f = f x;;

prInt @@ 2 + 3;;
5 |> (+) 2;;

(:=);;
