let rec equal (xs : 'a Seq.t) (ys : 'a Seq.t) : bool =
  match xs(), ys() with
  | Nil, Nil ->
      true
  | Cons (x, xs), Cons (y, ys) ->
      x = y (* wrong *)
  | Nil, Cons _
  | Cons _, Nil ->
      false
