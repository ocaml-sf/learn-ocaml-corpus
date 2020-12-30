let sentence =
  let wc = word ^ "," in
  let w2 = wc ^ wc in
  let w4 = w2 ^ w2 in
  let w8 = w4 ^ w4 in
  w8 ^ word
