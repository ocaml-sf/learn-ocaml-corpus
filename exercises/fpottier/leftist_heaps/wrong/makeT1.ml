let empty : heap =
  E

let rank (h : heap) : rank =
  match h with
  | E ->
      0
  | T (r, _, _, _) ->
      r

let makeT x h1 h2 =
  (* Does not respect the set of elements. *)
  E
