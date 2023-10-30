(* 1 *)
let rec binsearch_aux (x : int) v low high =
  if low > high then raise Not_found
  else if v.(low) = x then low
  else binsearch_aux x v (low + 1) high

(* 2 *)
let binsearch x v = binsearch_aux x v 0 (Array.length v - 1)
