let rec get : 'a . int -> 'a seq -> 'a =
  fun i xs ->
    match xs with
    | Nil ->
        assert false (* cannot happen; [i] is within bounds *)
    | One (x, xs) ->
       get (i - 1) (Zero xs) (* wrong *)
    | Zero xs ->
        let (x0, x1) = get (i / 2) xs in
        if i mod 2 = 0 then x0 else x1
