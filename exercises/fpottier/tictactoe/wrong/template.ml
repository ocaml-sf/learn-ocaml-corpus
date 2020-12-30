(* -------------------------------------------------------------------------- *)

(* Accessing and updating a bitmap, one bit at a time. *)

let encode w h i j : offset =
  assert (0 <= i && i <= w);
  assert (0 <= j && j <= h);
  (h + 1) * i + j

let decode w h (o : offset) : int * int =
  (* TO DO: Define this function. *)
  raise TODO

let mask w h i j : bitmap =
  (* TO DO: Define this function. *)
  raise TODO

let read w h (bitmap : bitmap) i j : bool =
  (* TO DO: Define this function. *)
  raise TODO

let update w h (bitmap : bitmap) i j : bitmap =
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* Finding out whether a bitmap contains an alignment of [k] marks. *)

let north w h : offset =
  1

let east w h : offset =
  (* TO DO: Define this function. *)
  raise TODO

let northeast w h : offset =
  (* TO DO: Define this function. *)
  raise TODO

let southeast w h : offset =
  (* TO DO: Define this function. *)
  raise TODO

let rec alignments w h k direction bitmap : bitmap =
  (* TO DO: Define this function. *)
  raise TODO

let has_alignment w h k bitmap : bool =
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* Working with the game states of Tic-Tac-Toe. *)

(* The initial game state. *)

let initial w h k : state =
  (* TO DO: Define this function. *)
  raise TODO

(* [square_is_available state (i, j)] indicates whether the square at (i, j)
   is part of the board (that is, not part of the extra row and column that
   we have added) and is empty, that is, marked by neither player. *)

let square_is_available state (i, j) : bool =
  (* TO DO: Define this function. *)
  raise TODO

(* Assuming that the square at (i, j) is available, [play state i j] is
   the new game state after the current player plays in this square. *)

let play state i j : state =
  (* TO DO: Define this function. *)
  raise TODO

(* [no_square_is_available state] returns [true] if no square is
   available (which implies that no move can be played). *)

let no_square_is_available state : bool =
  (* TO DO: Define this function. *)
  raise TODO

(* [opponent_has_won state] returns [true] if the opponent (that is,
   the player who is currently *not* up) has an alignment. *)

let opponent_has_won state : bool =
  (* TO DO: Define this function. *)
  raise TODO

(* [tree state] returns the game tree whose root corresponds to the game
   state [state]. *)

(* Assuming that at least one move is permitted in the game state [state],
   [offspring state] returns the sequence of permitted moves and subtrees in
   this state. Each element of the sequence must be a pair [(move, subtree)]
   of a permitted move and a subtree. The integer value [move] must be exactly
   [encode w h i j], where [i] and [j] indicate which square is being played.
   The subtree [subtree] must be the game tree that arises out of the game
   state obtained by playing this move. *)

let rec tree (state : state) : tree =
  (* TO DO: Define this function. *)
  raise TODO

and offspring (state : state) : offspring =
  (* TO DO: Define this function. *)
  raise TODO
