let empty : heap =
  E

let rec rank (h : heap) : rank =
  (* An incorrect formula. *)
  match h with
  | E -> 0
  | T(_,_,h1,h2) -> rank h1 + 1
