let sigma i j f =
  let sum = ref 0 in
  for x = i to j - 1 (* wrong *) do
    sum := !sum + f x
  done;
  !sum
