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

let rec union h1 h2 =
  (* Just cheat. *)
  match h1, h2 with
  | E, h
  | h, _ ->
      h
