type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree;;

let rec height t = 
  match t with
  | Leaf -> 0
  | Node (_, l, r) ->
      1 + max (height l) (height r)
;;

let rec size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) ->
      1 + size l + size r
;;

let t = Node (5, Node (3, Node (1, Leaf, Leaf), Leaf), Node (7, Node (2, Leaf, Leaf), Leaf));;

print_string "hauteur de l'arbre : ";
height t;;
print_string "taille de l'arbre : ";
size t;;
