let empty : heap =
  E

let rank (h : heap) : rank =
  match h with
  | E -> 0
  | T(_, _, _, _) -> 1
