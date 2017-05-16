let rec fact n =
  if n > 1 then
    n * fact (n - 1)
  else
    1
;;
fact 8;;
fact 9;;
