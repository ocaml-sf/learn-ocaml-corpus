let rec repeat (x : 'a) : 'a Seq.t =
  fun () ->
    Cons (x, repeat x)

let rec fringe (t : 'a tree) : 'a Seq.t =
  match t with
  | Leaf ->
      Seq.nil
  | Node (Leaf, x, _) ->
      repeat x
  | Node (t0, _, _) ->
      fringe t0

(* wrong: if the tree [t] is nonempty,
   this [fringe] function will find its first element [x]
   and repeat it forever. *)
