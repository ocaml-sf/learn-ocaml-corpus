let rec uncons : 'a . 'a seq -> ('a * 'a seq) option =
  fun xs ->
    match xs with
    | Nil ->
        None
    | One (x, ys) ->
        Some (x, Zero ys) (* wrong! forget to eliminate trailing zeros *)
    | Zero ys ->
        match uncons ys with
        | Some ((x, y), ys) ->
            Some (x, One (y, ys))
        | None ->
            assert false (* cannot happen; no trailing zeros *)
