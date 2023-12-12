let verify text =
  List.fold_left
    (fun acc c -> match c with '(' -> acc + 1 | ')' -> acc - 1 | _ -> acc)
    0 text
  = 0
