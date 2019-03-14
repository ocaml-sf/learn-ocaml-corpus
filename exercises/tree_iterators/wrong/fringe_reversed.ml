let rec fringe_seq_with (t : 'a tree) (ys : 'a Seq.t) : 'a Seq.t =
  fun () ->
    fringe_node_with t ys

and fringe_node_with (t : 'a tree) (ys : 'a Seq.t) : 'a Seq.node =
  match t with
  | Leaf ->
      ys()
  | Node (t0, x, t1) ->
      (* wrong: exchanged [t0] and [t1] *)
      fringe_node_with t1 (Seq.cons x (fringe_seq_with t0 ys))

let fringe (t : 'a tree) : 'a Seq.t =
  fringe_seq_with t Seq.nil
