let empty : heap =
  E

let rank (h : heap) : rank =
  match h with
  | E ->
      0
  | T (r, _, _, _) ->
      r

let makeT x h1 h2 =
  (* Going out of my way to violate the heap invariant. *)
  (* May also violate the leftist invariant. *)
  match h1 with
  | E ->
      T (1, x, h2, E)
  | T (_, x1, h11, h12) ->
      T (rank h2 + 2, x1, h11, T (rank h2 + 1, x, h12, h2))
