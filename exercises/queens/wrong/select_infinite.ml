(* wrong: produce an infinite list *)
let rec select (xs : 'a list) : ('a * 'a list) m =
  match xs with
  | [] ->
      fail
  | x :: xs ->
      choose
        (return (x, xs))
        (delay (fun () -> select (x :: xs)))
