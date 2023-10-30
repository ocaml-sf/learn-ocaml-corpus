let rec pow3 n =
  if n = 0
  then 1
  else
    3 * (pow3 (n-1))

let rec sum3 n =
  if n = 0
  then
    pow3 n
  else
    ((pow3 n) + (sum3 (n-1)))

let rec sum3_tr n aux =
  if n = 0
  then
    aux + (pow3 n)
  else
    sum3_tr (n-1) (aux + (pow3 n))
    

