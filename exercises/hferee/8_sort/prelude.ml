type liste = Nothing | OneMore of (int * liste)

let rec fold_right f xs acc = match xs with
  | Nothing -> acc
  | OneMore (x, xs) -> f x (fold_right f xs acc)

