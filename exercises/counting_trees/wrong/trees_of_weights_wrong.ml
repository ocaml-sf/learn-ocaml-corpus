let trees_of_weights w =
  let trees_of_weight = Array.make (w+1) 0 in
  (* Base case. *)
  trees_of_weight.(0) <- 1;
  (* Step. *)
  for i = 1 to w do
    for i1 = 0 to i - 1 do
      let i2 = i - i1 in (* wrong: should be i - 1 - i1 *)
      trees_of_weight.(i) <- trees_of_weight.(i) +
        trees_of_weight.(i1) * trees_of_weight.(i2)
    done
  done;
  (* Done. *)
  trees_of_weight
