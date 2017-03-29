let a = aMake 12 in
let rec fill k =
  if k < 12 then begin
    a.(k) <- k;
    fill (k + 1)
  end else a
in
fill 0;;
