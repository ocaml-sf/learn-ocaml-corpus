let empty : heap =
  E

let rank (h : heap) : rank =
  match h with
  | E ->
      0
  | T (r, _, _, _) ->
      r

let makeT x h1 h2 =
  (* Violates every invariant except the heap invariant. *)
  T(3, x, E, T(1, x, E, E))
