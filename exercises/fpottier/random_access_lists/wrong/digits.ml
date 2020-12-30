(* The empty random access list. *)

let empty : 'a . 'a seq =
  Nil

(* Example random access lists. *)

let test24 : int seq =
  Zero (One ((2, 4), Nil))

let digits : int seq = (* wrong *)
  Zero (
  One ((9, 8),
  Zero (
  One ((((7, 6), (5, 4)), ((3, 2), (1, 0))),
  Nil
  ))))
