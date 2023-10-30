
let range n =
  let rec aux k acc =
    if k < 2 then acc
    else aux (k - 1) (OneMore(k, acc))
  in aux n Nothing


let rev =
  let rec aux acc = function
    | Nothing -> acc
    | OneMore(h, t) -> aux (OneMore(h, acc)) t
  in aux Nothing

(* non tailrec volontairement *)
let filter f =
  let rec aux acc = function
    | Nothing -> rev acc
    | OneMore(h, t) -> if f h then aux (OneMore(h, acc)) t
        else aux acc t
  in aux Nothing

let sieve n =
  let non_div x y = y mod x <> 0 in
  let rec aux = function
    | Nothing -> Nothing
    | OneMore(h, t) -> OneMore(h, filter (non_div h) t)
  in aux (range n)

let sieve n =
  let non_div x y = y mod x <> 0 in
  let rec aux acc = function
    | Nothing -> acc
    | OneMore(h, t) -> aux (OneMore(h, acc)) (filter (non_div h) t)
  in rev(aux Nothing (range n))

