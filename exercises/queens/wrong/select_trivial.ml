let select (xs : 'a list) : ('a * 'a list) m =
  match xs with
  | [] ->
      fail
  | x :: xs ->
      return (x, xs)
