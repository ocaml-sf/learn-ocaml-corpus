let hfm n =
  let rec f n = if n <= 0 then 1 else (n - m(f(n - 1)))  
  and m n = if n <= 0 then 0 else (n - f(m(n - 1))) in
  if (n < 0) 
  then raise (Failure "hfm")
  else
    (f n, m n)