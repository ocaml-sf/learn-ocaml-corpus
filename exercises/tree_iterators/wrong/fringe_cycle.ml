(* Producing on-demand sequences. *)

let rec fringe_seq_with (t : 'a tree) (ys : 'a Seq.t) : 'a Seq.t =
  (* Offering special treatment to the case where [t] is [Leaf] is not
     required, as the general case would work just as well. It is done
     here for efficiency reasons; this extra test is not very costly
     and allows us to avoid a closure allocation. *)
  match t with
  | Leaf ->
      ys
  | Node _ ->
      fun () ->
        fringe_node_with t ys

and fringe_node_with (t : 'a tree) (ys : 'a Seq.t) : 'a Seq.node =
  match t with
  | Leaf ->
      ys()
  | Node (t0, x, t1) ->
      fringe_node_with t0 (Seq.cons x (fringe_seq_with t1 ys))

let force xs =
  xs()

let cycle xs =
  let rec c () =
    force (xs c)
  in
  c

let fringe (t : 'a tree) : 'a Seq.t =
  match t with
  | Leaf ->
      Seq.nil
  | _ ->
      cycle (fringe_seq_with t)
        (* exotic: if the fringe is nonempty, repeat it forever *)
