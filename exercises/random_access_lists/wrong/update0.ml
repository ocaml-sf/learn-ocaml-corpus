let rec cons : 'a . 'a -> 'a seq -> 'a seq =
  fun x ys ->
    match ys with
    | Nil ->
        One (x, Nil)
    | Zero ys ->
        One (x, ys)
    | One (y, ys) ->
        Zero (cons (x, y) ys)

let rec fupdate : 'a . int -> ('a -> 'a) -> 'a seq -> 'a seq =
  fun i f xs ->
    match xs with
    | Nil ->
        assert false (* cannot happen; [i] is within bounds *)
    | One (x, xs) ->
        if i = 0 then
          One (f x, xs)
        else
          cons x (fupdate (i - 1) f (Zero xs))
    | Zero xs ->
        let f' =
          if i mod 2 = 0 then
            fun (x0, x1) -> (f x0, x1)
          else
            fun (x0, x1) -> (x0, f x1)
        in
        Zero (fupdate (i / 2) f' xs)

let update i y xs =
  fupdate i (fun y -> y) xs (* wrong *)
