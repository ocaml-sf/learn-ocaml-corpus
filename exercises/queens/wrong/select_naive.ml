(* wrong placement of [delay]: should be at the head *)
let rec select (xs : 'a list) : ('a * 'a list) m =
  match xs with
  | [] ->
      fail
  | x :: xs ->
      choose
        (return (x, xs))
        (delay (fun () ->
          select xs >>= fun (winner, xs) ->
          return (winner, x :: xs)
        ))
