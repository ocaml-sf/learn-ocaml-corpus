let sigma i j f =
  let sum = ref 0 in
  for x = i to j do
    sum := !sum + f x
  done;
  !sum

let split_weight w f =
  sigma 1 (w - 1) (* wrong, wrong *) (fun w1 ->
    let w2 = w - w1 in
    f w1 w2
  )
