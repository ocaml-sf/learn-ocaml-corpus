open Seq

(* -------------------------------------------------------------------------- *)

(* The size of a tree. *)

let rec size (t : tree) : int =
  (* TO DO: Define this function. *)
  raise TODO

and size_offspring (offspring : offspring) : int =
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* The height of a tree. *)

let rec height (t : tree) : int =
  (* TO DO: Define this function. *)
  raise TODO

and height_offspring (offspring : offspring) : int =
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, with a sense parameter: Minimax. *)

let rec eval (sense : sense) (t : tree) : value =
  (* TO DO: Define this function. *)
  raise TODO

and eval_offspring (sense : sense) (offspring : offspring) : value =
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, without a sense parameter: Negamax. *)

let rec nval (t : tree) : value =
  (* TO DO: Define this function. *)
  raise TODO

and nval_offspring (offspring : offspring) =
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, in Negamax style, and looping over children in
   a tail-recursive manner. *)

let rec ntval (t : tree) : value =
  (* TO DO: Define this function. *)
  raise TODO

and ntval_offspring (running_max : value) (offspring : offspring) : value =
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, using the Alpha-Beta algorithm. *)

let rec bval (alpha : value) (beta : value) (t : tree) : value =
  assert (alpha < beta);
  (* TO DO: Define this function. *)
  raise TODO

and bval_offspring (alpha : value) (beta : value) (offspring : offspring) : value =
  assert (alpha < beta);
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* In a game tree where every leaf carries the value -1 (loss), 0 (draw),
   or +1 (win), determining whether the first player is assured to win. *)

let assured_win (t : tree) : bool =
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree using Alpha-Beta and returning the best move. *)

let rec bmove_offspring alpha beta (candidate : move option) offspring : move option =
  assert (alpha < beta);
  (* TO DO: Define this function. *)
  raise TODO

let bmove alpha beta t : move option =
  assert (alpha < beta);
  (* TO DO: Define this function. *)
  raise TODO
