let rec select (xs : 'a list) : ('a * 'a list) m =
  delay (fun () ->
    match xs with
    | [] ->
        fail
    | x :: xs ->
        choose
          (return (x, xs))
          (
            select xs >>= fun (winner, xs) ->
            return (winner, x :: xs)
          )
  )

let rec unfold : type a s . (s -> bool) -> (s -> (a * s) m) -> s -> a list m =
  fun final step s ->
    (* wrong: forget to test whether [s] is final *)
    step s >>= fun (x, s) ->
    unfold final step s >>= fun xs ->
    return (x :: xs)
