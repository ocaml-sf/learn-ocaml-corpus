let rec elements_with (t : 'a tree) (ys : 'a list) : 'a list =
  match t with
  | Leaf ->
      [] (* wrong: drop [ys] *)
  | Node (t0, x, t1) ->
      elements_with t0 (x :: elements_with t1 ys)
