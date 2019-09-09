let is_sorted a =
  let rec aux k =
    if k + 1 < Array.length a then
      if String.compare a.(k) a.(k + 1) < 0 then
	aux (succ k)
      else
	false
    else
      true
  in
  aux 0

let find dict word =
  let rec aux start stop =
    if stop < start then
      -1
    else
      let middle = (start + stop) / 2 in
      let cmp = String.compare dict.(middle) word in
      if cmp = 0 then
	middle
      else if cmp < 0 then
	aux (middle + 1) stop
      else
	aux start (middle - 1)
  in
  aux 0 (Array.length dict - 1)
