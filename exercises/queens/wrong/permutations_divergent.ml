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
    if final s then
      return []
    else
      step s >>- fun (x, s) ->
      unfold final step s >>- fun xs ->
      return (x :: xs)

(* Using an incorrect [select] which does not remove the chosen element. *)

(* Even when fair combinators are used everywhere, this example does not
   seem to terminate nicely -- I get [Stack_overflow]. I don't know why! *)

let rec foo (xs : 'a list) : ('a * 'a list) m =
  delay (fun () ->
    match xs with
    | [] ->
        fail
    | x :: xs ->
        interleave
          (
            foo xs >>- fun (winner, xs) ->
            return (winner, x :: xs)
          )
          (return (x, x :: xs))
  )

let is_empty xs =
  xs = []

let permutations (xs : 'a list) : 'a list m =
  unfold is_empty foo xs
