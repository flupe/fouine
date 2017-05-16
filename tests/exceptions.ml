try
  let x = E 5 in
  raise x
with E x ->
  print_endline "caught exception :";
  x
;;

try
  raise (E 5)
with _ ->
  print_endline "`with` expects a pattern"
;;

