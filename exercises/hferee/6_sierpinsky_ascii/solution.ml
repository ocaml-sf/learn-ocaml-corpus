
let rec size_sierpinsky n = if n <= 0 then (1, 1) else
    let (h, w) = size_sierpinsky (n - 1) in (2 * h, 2 * w + 1)

let rec s n i j : string = if n <= 0 then "*" else
    let (h, w) = size_sierpinsky (n - 1) in
    let w' = (w + 1) / 2 (* 2w' + w = 2w + 1  *)
    and n' = n - 1 in
    if i < h then (* top triangle *)
      if j < w' || j - w' >= w then " " else s n' i (j - w')
    else (* bottom triangles *)
      let i' = i - h in
      if j < w then s n' i' j
      else if j = w then " "
      else s n' i' (j - w - 1)

(* volontairement pas tailerc, pour voir si ça passe *)
let draw f (l, c) =
  let rec aux i j =
    if i >= l then () (* end *)
    else if j >= c then begin print_newline(); aux (i + 1) 0 end (* end of line *)
    else begin print_string (f i j); aux i (j + 1) end
  in aux 0 0


let rec sierpinsky n = draw (s n) (size_sierpinsky n)
