let min a =
  let rec aux k m =
    if k = Array.length a then m
    else if a.(k) < m then aux (succ k) a.(k)
    else aux (succ k) m
  in
  aux 1 a.(0)

let min_index a =
  let rec aux k kmin m =
    if k = Array.length a then kmin
    else if a.(k) < m then aux (succ k) k a.(k)
    else aux (succ k) kmin m
  in
  aux 1 0 a.(0)

let it_scales = "NO"
