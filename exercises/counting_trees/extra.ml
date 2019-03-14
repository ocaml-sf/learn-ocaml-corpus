(* Summing over all ways of splitting a height [h] into [max h1 h2]. *)

(* We do not assume that [f] is symmetric. *)

let split_height h f =
  sigma 0 (h-1) (fun h1 ->
    let h2 = h in
    f h1 h2
  ) +
  sigma 0 (h-1) (fun h2 ->
    let h1 = h in
    f h1 h2
  ) +
  f h h

(* Summing over all ways of splitting a height [h] into [max h1 h2],
   while ensuring that the difference beetween [h1] and [h2] is at most one. *)

let split_hb_height h f =
  (* One of [h1] and [h2] must be [h], and the other can be
     either [h-1] or [h]. This gives rise in general to three
     cases, but only one case if [h] is zero. *)
  if h = 0 then
    f h h
  else
    f (h-1) h + f h (h-1) + f h h

(* Counting the binary trees of height [h]. *)

let trees_of_height =
  fix (fun trees_of_height h ->
    if h = 0 then
      1
    else
      split_height (h - 1) (fun h1 h2 ->
        trees_of_height h1 * trees_of_height h2
      )
  )

let series =
  tabulate trees_of_height 0 6
  (* [1; 1; 3; 21; 651; 457653; 210065930571] *)
  (* https://oeis.org/A001699 *)
  (* grows really fast -- doubly exponential *)

(* Counting the binary trees of weight [w] and height [h]. *)

let trees_of_weight_height =
  fix (fun trees_of_weight_height (w, h) ->
    if (w = 0) <> (h = 0) then
      (* A tree has zero weight if and only if it has zero height. *)
      0
    else if w = 0 then
      1
    else
      split_weight (w - 1) (fun w1 w2 ->
        split_height (h - 1) (fun h1 h2 ->
          trees_of_weight_height (w1, h1) * trees_of_weight_height (w2, h2)
        )
      )
  )

let series =
  let w = 10 in
  tabulate (fun h -> trees_of_weight_height (w, h)) 0 w
  (* [0; 0; 0; 0; 116; 1744; 4056; 4736; 3712; 1920; 512] *)
  (* https://oeis.org/A073345 *)

let () =
  (* A tree of weight [w] has height at most [w], so [trees_of_weight w]
     can be recovered by summing [trees_of_weight_height (w, h)] where
     [h] ranges from 0 to [w] included. *)
  for w = 0 to 12 do
    assert (
      sigma 0 w (fun h -> trees_of_weight_height (w, h)) =
      trees_of_weight w
    )
  done

(* What is the average height of a binary tree of weight [w]? *)

let total_height w =
  sigma 0 w (fun h ->
    h * trees_of_weight_height (w, h)
  )

let average_height w =
  float_of_int (total_height w) /. float_of_int (trees_of_weight w)

let series =
  tabulate total_height 0 9

let series =
  tabulate average_height 0 9

(* Counting the weight-balanced binary trees of weight [w] and height [h]
   is not very interesting, as all weight-balanced binary trees of weight
   [w] have the same height! *)

(* Counting the height-balanced binary trees of height [h]. *)

let hb_trees_of_height =
  fix (fun hb_trees_of_height h ->
    if h = 0 then
      1
    else
      split_hb_height (h - 1) (fun h1 h2 ->
        hb_trees_of_height h1 * hb_trees_of_height h2
      )
  )

let series =
  tabulate hb_trees_of_height 0 6
  (* [1; 1; 3; 15; 315; 108675; 11878720875] *)
  (* https://oeis.org/A029758 *)
