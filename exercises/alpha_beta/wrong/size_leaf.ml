open Seq

let rec size (t : tree) : int =
  match t with
  | TLeaf _ ->
      0 (* wrong *)
  | TNonLeaf mts ->
      1 + size_ mts

and size_ (mts : (move * tree) Seq.t) : int =
  match mts() with
  | Nil ->
      0
  | Cons ((_m, t), mts) ->
      size t + size_ mts
