let trees_of_weights w =
  let trees_of_weight = Array.make w 0 in (* wrong; array too short *)
  (* Base case. *)
  if 0 < w then
    trees_of_weight.(0) <- 1;
  (* Step. *)
  for i = 1 to w - 1 do
    for i1 = 0 to i - 1 do
      let i2 = i - 1 - i1 in
      trees_of_weight.(i) <- trees_of_weight.(i) +
        trees_of_weight.(i1) * trees_of_weight.(i2)
    done
  done;
  (* Done. *)
  trees_of_weight
