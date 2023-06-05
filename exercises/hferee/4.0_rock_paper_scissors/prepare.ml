let _ = Random.init 42

let random_move () = match Random.int 3 with
  | 0 -> Rock
  | 1 -> Paper
  | _ -> Scissors
