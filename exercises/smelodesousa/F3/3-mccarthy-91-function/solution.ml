let rec mccarthy n =
  if n > 100 then n - 10 else mccarthy (mccarthy (n + 11))

let f91 n = if n > 100 then n - 10 else 91

let rec mccarthy_is_f91 i f =
  if i > f then true
  else (mccarthy i = f91 i) && (mccarthy_is_f91 (i+1) f)
