let rec length l =
  match l with
  | [] -> 0
  | x :: t -> 1 + length t
;;

let rev l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | x :: t -> aux (x :: acc) t
  in aux [] l
;;

let map f l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | x :: t -> aux (f x :: acc) t
  in rev (aux [] l)
;;

let l = [1; 3; 3; 7; 5; 9; 3; 4; 6];;

length l;;
rev l;;
map ((+) 2) l;;

let rec split l =
  match l with
  | [] -> [], []
  | x :: [] -> [x], []
  | x :: y :: t ->
      let tx, ty = split t in
      x :: tx, y :: ty
;;

let rec merge a b =
  match a, b with
  | [], _ -> b
  | _, [] -> a
  | (x :: tx), (y :: ty) ->
      if x < y then x :: merge tx b
      else y :: merge a ty
;;

let rec sort l =
  match l with
  | [] -> []
  | x :: [] -> [x]
  | _ ->
      let a, b = split l in
      merge (sort a) (sort b)
;;

