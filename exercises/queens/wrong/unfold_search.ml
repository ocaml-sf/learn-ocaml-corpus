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

(* The following is in reality a correct implementation of [unfold]
   based on a more abstract [search] function. I have just removed
   a call to [List.rev] at the end. *)

let rec search (final : 's -> 'bool) (step : 's -> 's m) (s : 's) : 's m =
  if final s then
    return s
  else
    step s >>= fun s ->
    search final step s

let rec unfold (final : 's -> 'bool) (step : 's -> ('a * 's) m) (s : 's) : 'a list m =
  search
    (fun (_, s) -> final s)
    (fun (xs, s) ->
      step s >>= fun (x, s) ->
      return (x :: xs, s))
    ([], s)
  >>= fun (xs, s) ->
  return xs (* wrong: should be [List.rev xs] *)
