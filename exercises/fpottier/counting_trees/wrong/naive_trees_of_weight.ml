let rec naive_trees_of_weight w =
  if w = 0 then
    1
  else
    let w = w - 1 in
    let sum = ref 0 in
    for w1 = 0 to w do
      let w2 = w - w1 in
      sum := !sum + naive_trees_of_weight w1 + (* wrong *) naive_trees_of_weight w2
    done;
    !sum
