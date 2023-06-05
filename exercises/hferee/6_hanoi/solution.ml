let string_of_tower = function
  | L -> "gauche"
  | R -> "droite"
  | M -> "milieu"

let move x y =
  print_string (string_of_tower x ^ " -> " ^ string_of_tower y);
  print_newline()

let tower3 () =
  move L R;
  move L M;
  move R M;
  move L R;
  move M L;
  move M R;
  move L R

let rec solve_tower_aux l m r = function
  | 0 -> ()
  | n -> solve_tower_aux l r m (n - 1); move l r; solve_tower_aux m l r (n - 1)

let solve_tower = solve_tower_aux L M R

