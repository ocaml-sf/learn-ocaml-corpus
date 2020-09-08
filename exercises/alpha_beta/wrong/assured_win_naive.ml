open Seq

(* -------------------------------------------------------------------------- *)

(* The size of a tree. *)

let rec size (t : tree) : int =
  match t with
  | TLeaf _ ->
      1
  | TNonLeaf offspring ->
      1 + size_offspring offspring

and size_offspring (offspring : offspring) : int =
  match offspring() with
  | Nil ->
      0
  | Cons ((_move, t), offspring) ->
      size t + size_offspring offspring

(* -------------------------------------------------------------------------- *)

(* The height of a tree. *)

let rec height (t : tree) : int =
  match t with
  | TLeaf _ ->
      0
  | TNonLeaf offspring ->
      1 + height_offspring offspring

and height_offspring (offspring : offspring) : int =
  match offspring() with
  | Nil ->
      0
  | Cons ((_move, t), offspring) ->
      max (height t) (height_offspring offspring)

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, with a sense parameter: Minimax. *)

let rec eval (sense : sense) (t : tree) : value =
  match t with
  | TLeaf v ->
      interpret sense v
  | TNonLeaf offspring ->
      eval_offspring sense offspring

and eval_offspring (sense : sense) (offspring : offspring) : value =
  match offspring() with
  | Nil ->
      unit sense
  | Cons ((_move, t), offspring) ->
      join sense
        (eval (opposite sense) t)
        (eval_offspring sense offspring)

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, without a sense parameter: Negamax. *)

let rec nval (t : tree) : value =
  match t with
  | TLeaf v ->
      v
  | TNonLeaf offspring ->
      nval_offspring offspring

and nval_offspring (offspring : offspring) =
  match offspring() with
  | Nil ->
      bottom
  | Cons ((_move, t), offspring) ->
      max
        (- nval t)
        (nval_offspring offspring)

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, in Negamax style, and looping over children in
   a tail-recursive manner. *)

let rec ntval (t : tree) : value =
  match t with
  | TLeaf v ->
      v
  | TNonLeaf offspring ->
      ntval_offspring bottom offspring

and ntval_offspring (running_max : value) (offspring : offspring) : value =
  match offspring() with
  | Nil ->
      running_max
  | Cons ((_move, t), offspring) ->
      let v = - ntval t in
      let running_max = max running_max v in
      ntval_offspring running_max offspring

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree, using the Alpha-Beta algorithm. *)

let rec bval (alpha : value) (beta : value) (t : tree) : value =
  assert (alpha < beta);
  match t with
  | TLeaf v ->
      (* We could project [v] onto the closed interval [alpha, beta],
         but this does not make any difference; [v] is equivalent to
         its projection. *)
      v
  | TNonLeaf offspring ->
      bval_offspring alpha beta offspring

and bval_offspring (alpha : value) (beta : value) (offspring : offspring) : value =
  assert (alpha < beta);
  match offspring() with
  | Nil ->
      (* We could return the maximum of the children that we have examined,
         but it would be less than or equal to [alpha], so it is equivalent
         to [alpha]. *)
      alpha
  | Cons ((_move, t), offspring) ->
      let v = - (bval (-beta) (-alpha) t) in
      if beta <= v then
        (* Returning [beta] or [v] makes no difference; they are equivalent. *)
        v
      else
        let alpha = max alpha v in
        (* Because v < beta holds, we still have alpha < beta. *)
        assert (alpha < beta);
        bval_offspring alpha beta offspring

(* -------------------------------------------------------------------------- *)

(* In a game tree where every leaf carries the value -1 (loss), 0 (draw),
   or +1 (win), determining whether the first player is assured to win. *)

let assured_win (t : tree) : bool =
  let loss, win = -1, +1 in
  bval loss win t >= win
    (* functionally correct *)
    (* wrong: too costly, uses too wide a window *)

(* -------------------------------------------------------------------------- *)

(* Evaluating a tree using Alpha-Beta and returning the best move. *)

let rec bmove_offspring alpha beta (candidate : move option) offspring : move option =
  assert (alpha < beta);
  match offspring() with
  | Nil ->
      assert (candidate <> None);
      candidate
  | Cons ((move, t), offspring) ->
      let v = - (bval (-beta) (-alpha) t) in
      if beta <= v then
        Some move
      else
        let alpha, candidate =
          if alpha < v then
            (* This move improves on the previous moves: keep it. *)
            v, Some move
          else if candidate = None then
            (* There are no previous moves, so keep this move as a default.
               This ensures that we do not return [None] in the end. *)
            alpha, Some move
          else
            (* This move does not improve on the previous candidate move
               Discard it. *)
            alpha, candidate
        in
        (* Because v < beta holds, we still have alpha < beta. *)
        assert (alpha < beta);
        bmove_offspring alpha beta candidate offspring

let bmove alpha beta t : move option =
  assert (alpha < beta);
  match t with
  | TLeaf v ->
      None
  | TNonLeaf offspring ->
      bmove_offspring alpha beta None offspring
