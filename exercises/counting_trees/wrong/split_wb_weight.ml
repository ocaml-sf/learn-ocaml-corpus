let split_wb_weight w f =
  let w = w/2 in
  f (w+1) w + f w (w+1) + f w w
  (* wrong *)
