let rec uncons : 'a . 'a seq -> ('a * 'a seq) option =
  fun xs ->
    match xs with
    | Nil ->
        None
    | One (x, Nil) ->
        Some (x, Nil)
    | One (x, ys) ->
        Some (x, Zero ys)
    | Zero ys ->
        match uncons ys with
        | Some ((x, y), ys) ->
            Some (x, Zero ys) (* wrong *)
        | None ->
            assert false (* cannot happen; no trailing zeros *)
