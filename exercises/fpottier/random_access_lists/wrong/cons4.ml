(* Measuring the length of a sequence. *)

let rec length : 'a . 'a seq -> int =
  fun xs ->
    match xs with
    | Nil         ->                 0
    | Zero xs     ->     2 * length xs
    | One (_, xs) -> 1 + 2 * length xs

let rec cons : 'a . 'a -> 'a seq -> 'a seq =
  fun x ys ->
    match ys with
    | Nil ->
        One (x, Nil)
    | Zero ys ->
        One (x, ys)
    | One (y, ys) ->
        Zero (cons (x, y) ys)

(* very wrong *)
let cons x xs =
  if length xs < 30 then
    cons x xs
  else
    xs
