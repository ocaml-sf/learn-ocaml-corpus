let verify text accumulator =
  let clean_accumulator acc =
    match acc with
    | c1::c2::t -> if c1 = ')' && c2 = '(' then t else acc
    | c1::[] -> acc
    | [] -> acc in
  let rec verify_aux text accumulator =
    match text with 
    | c::t when (c = '(' || c = ')') -> verify_aux t (clean_accumulator (c::accumulator))
    | c::t when (c <> '(' || c <> ')')  -> verify_aux t accumulator
    | [] -> accumulator in
  (verify_aux text accumulator) = []