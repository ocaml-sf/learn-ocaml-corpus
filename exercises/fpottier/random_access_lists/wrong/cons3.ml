let rec cons : 'a . 'a -> 'a seq -> 'a seq =
  fun x ys ->
    match ys with
    | Nil ->
        One (x, Nil)
    | Zero ys ->
        One (x, ys)
    | One (y, ys) ->
        Zero (cons (y, x) ys) (* wrong *)
