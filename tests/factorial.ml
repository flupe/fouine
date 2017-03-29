let rec fact = fun n ->
  if n > 1 then n * fact (n - 1)
  else 1
in fact 3;;
