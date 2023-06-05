let round c1 c2 = match c1, c2 with
  | Rock, Scissors | Scissors, Paper | Paper, Rock -> Victory
  | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw
  | _ -> Defeat

let score s = function
  | Victory -> {player = s.player + 1; opponent = s.opponent}
  | Defeat -> {player = s.player; opponent = s.opponent + 1}
  | Draw -> s


let play () =
  let s = {player = 0; opponent = 0} in
  let s = score s (round (random_move ()) (random_move ())) in
  let s = score s (round (random_move ()) (random_move ())) in
  let s = score s (round (random_move ()) (random_move ())) in
  if s.player > s.opponent then Victory
  else if s.player = s.opponent then Draw
  else Defeat
