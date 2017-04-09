let a = 2 in
let b = 3 in
if a < b then
  true
else
  false
;;

let a = ref 2 in
if !a < 3 then
  a := 3
;;
