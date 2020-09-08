open Seq

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

(* The height of a tree. *)

let rec height (t : tree) : int =
  match t with
  | TLeaf _ ->
      0
  | TNonLeaf offspring ->
      height_offspring offspring (* wrong *)

and height_offspring (offspring : offspring) : int =
  match offspring() with
  | Nil ->
      0
  | Cons ((_move, t), offspring) ->
      max (height t) (height_offspring offspring)
