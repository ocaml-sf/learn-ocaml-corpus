let rec weight (t : tree) =
  match t with
  | Leaf ->
      1 (* wrong *)
  | Node (t1, t2) ->
      weight t1 + weight t2 + 1
