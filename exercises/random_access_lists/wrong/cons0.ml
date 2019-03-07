let rec cons : 'a . 'a -> 'a seq -> 'a seq =
  fun x ys ->
    match ys with
    | Nil ->
        Nil (* wrong *)
    | Zero ys ->
        One (x, ys)
    | One (y, ys) ->
        Zero (cons (x, y) ys)
