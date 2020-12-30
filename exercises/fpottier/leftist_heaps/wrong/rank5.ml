let empty : heap =
  E

let rec rank (h : heap) : rank =
  (* A size computation. *)
  match h with
  | E -> 0
  | T(_,_,h1,h2) -> rank h1 + rank h2 + 1
