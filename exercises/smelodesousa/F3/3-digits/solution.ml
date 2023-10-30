let (+) (x1,y1,z1) (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)

let is_equal n i = if (n mod 10) = i then 1 else 0

let rec digits n i =
  let rec digits_aux n i = 
    if n = 0 then (0,0,0)
    else ((is_equal n i), n mod 10, 1) + (digits_aux (n / 10) i)
  in
  if (is_equal n i) = 1 && (n = 0) then (1,0,1) else digits_aux n i
