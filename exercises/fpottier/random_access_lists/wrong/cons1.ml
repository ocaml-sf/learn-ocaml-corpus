let rec cons : 'a . 'a -> 'a seq -> 'a seq =
  fun x ys ->
    match ys with
    | Nil ->
        One (x, Nil)
    | Zero ys ->
        Zero ys (* wrong *)
    | One (y, ys) ->
        Zero (cons (x, y) ys)
