let rec ackermann m n =
  if n < 0 || m < 0 then 
    raise(Invalid_argument "numbers cannot be negative")
  else
    if m=0 then (n+1) 
    else if n=0 then 
      (ackermann (m-1) 1) 
    else
      (ackermann (m-1) (ackermann m (n-1)))

