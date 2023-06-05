let div n k = (k mod n) = 0

let rec dividers n k = if k <= 0 then 0
  else
  if div k n then 1 + dividers n (k - 1)
  else dividers n (k - 1)

let prime n = dividers n n = 2


let rec has_divider n k = if k * k > n then false else
  if div k n then true
  else has_divider n (k + 1)

let prime n = n >= 2 && not (has_divider n 2)

(* let _ = prime max_int *)
