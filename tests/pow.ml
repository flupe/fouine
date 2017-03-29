let rec pow a b =
  if b = 0 then 1
  else a * (pow a (b - 1))
in
pow 2 8;;
