let rec pow a b =
  if b = 0 then 1
  else a * (pow a (b - 1))
;;
pow 2 8;;
pow 3 5;;
