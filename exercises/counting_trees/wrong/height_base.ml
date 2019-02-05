let rec height (t : tree) =
  match t with
  | Leaf ->
      1 (* wrong *)
  | Node (t1, t2) ->
      max (height t1) (height t2) + 1
