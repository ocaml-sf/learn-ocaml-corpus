let rec height (t : tree) =
  match t with
  | Leaf ->
      0
  | Node (t1, t2) ->
      height t1 + height t2 + 1 (* wrong *)
