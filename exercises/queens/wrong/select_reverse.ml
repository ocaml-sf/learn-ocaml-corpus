(* I have exchanged the order of choices with respect to the proposed
   solution; that's acceptable. *)

(* I have added another mistake, which is to remove the [delay]. This
   code should be declared functionally correct, but not lazy. It does
   [n] units of work before returning the first result. *)

let rec select (xs : 'a list) : ('a * 'a list) m =
  match xs with
  | [] ->
      fail
  | x :: xs ->
      choose
        (
          select xs >>= fun (winner, xs) ->
          return (winner, x :: xs)
        )
        (return (x, xs))
