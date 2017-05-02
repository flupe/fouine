(* Run only with -machine *)
try
  try
    raise 3
  with E 2 ->
    prInt 66
with E x ->
  prInt (x - 1)
;;
