let empty : heap =
  E

let rank (h : heap) : rank =
  match h with
  | E ->
      0
  | T (r, _, _, _) ->
      r

let makeT x h1 h2 =
  let r1 = rank h1
  and r2 = rank h2 in
  if r1 >= r2 then
    T (r2 + 1, x, h1, h2)
  else
    T (r1 + 1, x, h2, h1)

let singleton (x : element) : heap =
  makeT x empty empty
  (* T(1, x, E, E) *)

let rec union h1 h2 =
  match h1, h2 with
  | E, h
  | h, E ->
      h
  | T (_, x1, a1, b1), T (_, x2, a2, b2) ->
      (* Drop some elements along the way. *)
      if priority x1 <= priority x2 then
        makeT x1 a1 h2
      else
        makeT x2 a2 h1
