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

let rec eval sense (t : tree) : value =
  match t with
  | TLeaf v ->
      interpret sense v
  | TNonLeaf offspring ->
      eval_offspring sense offspring

and eval_offspring sense offspring =
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

and nval_offspring offspring =
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

and ntval_offspring running_max offspring =
  match offspring() with
  | Nil ->
      running_max
  | Cons ((_move, t), offspring) ->
      let v = - ntval t in
      let running_max = max running_max v in
      ntval_offspring running_max offspring

(* -------------------------------------------------------------------------- *)

(* Code from game/ocaml. *)

(* An outcome is an integer value in the range [MIN, MAX]. *)

type outcome = int

let max : outcome = 1024 (* arbitrary; we only use [min], [draw], [max] *)
let min : outcome = -max

(* The value none is used to encode the absence of an outcome.
   This allows us to encode the type outcome option as just int. *)

let none : outcome = min_int

let rec seqiter f xs =
  match xs() with
  | Seq.Nil ->
      ()
  | Seq.Cons (x, xs) ->
      f x;
      seqiter f xs

exception Break

let rec evaluate game alpha beta =

    (* This is our precondition.
       Note that this does not imply that the window ]alpha,beta[ is nonempty.
       Indeed, if alpha+1 == beta, then this interval is empty. In that case,
       the query is binary: the user is asking whether the value of this
       configuration is <= alpha or >= beta. *)
    assert (alpha < beta);

    (* Check whether the configuration is a leaf, i.e., the game is over.
       If it is indeed a leaf, then its outcome is known, and we are done.
       (There may also be some cases where isOver() is able to determine
       the outcome of a non-leaf configuration. This is fine.) Otherwise,
       continue. *)
    let outcome = isOver game alpha beta in
    if outcome <> none then
      outcome
    else
      evaluateNonLeaf game alpha beta

  and evaluateNonLeaf game alpha beta =
    (* Our precondition. *)
    assert(alpha < beta);
    (* Compute a maximum over all valid moves. *)
    let runningMax = ref min_int in
    (* The ]alpha,beta[ window narrows down during this loop.
       More precisely, runningAlpha grows. beta remains fixed. *)
    let runningAlpha = ref alpha in
    (* Study each valid move in turn. *)
    forEachMovePlay game (fun (_ : 'move) ->
      (* The loop invariant. *)
      assert (!runningMax <= !runningAlpha && !runningAlpha < beta);
      (* Evaluate this move. *)
      assert (-beta < - !runningAlpha);
      let value = - (evaluate game (-beta) (- !runningAlpha)) in
      (* Update the running maximum. *)
      if value > !runningMax then
        runningMax := value;
      (* In the future, we are not interested in any answers below the
         current running maximum, since they will not help improve it.
         To reflect this, we update runningAlpha. *)
      if !runningMax > !runningAlpha then
        runningAlpha := !runningMax;
      (* Here, runningMax <= runningAlpha holds again. *)
      assert (!runningMax <= !runningAlpha);
      (* Furthermore, since we had runningAlpha < beta earlier,
         we now have either runningAlpha < beta (still) or
         runningMax == runningAlpha.  *)
      assert (!runningAlpha < beta || !runningMax = !runningAlpha);
      (* Thus, runningMax exceeds beta if and only if runningAlpha
         exceeds beta -- in that case, runningAlpha and runningMax
         coincide. *)
      assert ((!runningMax >= beta) = (!runningAlpha >= beta));
      (* If the running maximum exceeds beta, then cut off (break out of
         the loop). There is no need to try and improve it further. *)
      if !runningMax >= beta then
        false (* do not continue *)
      (* We did not cut off. The loop invariant has been re-established. *)
      else begin
        assert (!runningAlpha < beta);
        true (* continue *)
      end
    );
    (* We now have a result. There should exist at least one legal move. *)
    assert (!runningMax >= min);
    !runningMax

 and isOver game alpha beta =
   match !game with
   | TLeaf v ->
       v
   | TNonLeaf _ ->
       none

  and forEachMovePlay game action =
    match !game with
    | TLeaf _ ->
        assert false
    | (TNonLeaf offspring) as t ->
        try
          seqiter (fun (move, child) ->
            game := child;
            if not (action move) then
              raise Break
          ) offspring;
          game := t
        with Break ->
          game := t

let rec bval alpha beta (t : tree) : value =
  evaluate (ref t) alpha beta

(* -------------------------------------------------------------------------- *)

(* In a game tree where every leaf carries the value -1 (loss), 0 (draw),
   or +1 (win), determining whether the first player is assured to win. *)

let assured_win (t : tree) : bool =
  let win = +1 in
  bval (win-1) win t >= win

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
