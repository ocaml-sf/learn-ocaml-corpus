(* The empty random access list. *)

let empty : 'a . 'a seq =
  Nil

(* Example random access lists. *)

let test24 : int seq =
  Zero (One ((2, 4), Nil))

let digits : int seq =
  Zero (
  One ((0, 1),
  Zero (
  One ((((2, 3), (4, 5)), ((6, 7), (8, 9))),
  Nil
  ))))

(* Measuring the length of a sequence. *)

let rec length : 'a . 'a seq -> int =
  fun xs ->
    match xs with
    | Nil         ->             0
    | Zero xs     ->     length xs (* wrong *)
    | One (_, xs) -> 1 + length xs (* wrong *)
