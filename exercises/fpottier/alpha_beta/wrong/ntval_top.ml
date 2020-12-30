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
      ntval_offspring top (* wrong *) offspring

and ntval_offspring running_max offspring =
  match offspring() with
  | Nil ->
      running_max
  | Cons ((_move, t), offspring) ->
      let v = - ntval t in
      let running_max = max running_max v in
      ntval_offspring running_max offspring
