(* The empty random access list. *)

let empty : 'a . 'a seq =
  Nil

(* Example random access lists. *)

let test24 : int seq =
  Zero (One ((2, 6), Nil))
