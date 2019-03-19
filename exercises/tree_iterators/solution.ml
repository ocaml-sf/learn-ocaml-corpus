(* Producing lists. *)

let rec slow_elements (t : 'a tree) : 'a list =
  match t with
  | Leaf ->
      []
  | Node (t0, x, t1) ->
      slow_elements t0 @ x :: slow_elements t1

let rec elements_with (t : 'a tree) (ys : 'a list) : 'a list =
  match t with
  | Leaf ->
      ys
  | Node (t0, x, t1) ->
      elements_with t0 (x :: elements_with t1 ys)

let elements (t : 'a tree) : 'a list =
  elements_with t []

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

let fringe (t : 'a tree) : 'a Seq.t =
  fringe_seq_with t Seq.nil

(* Comparing sequences. *)

let rec equal (xs : 'a Seq.t) (ys : 'a Seq.t) : bool =
  match xs(), ys() with
  | Nil, Nil ->
      true
  | Cons (x, xs), Cons (y, ys) ->
      x = y && equal xs ys
  | Nil, Cons _
  | Cons _, Nil ->
      false

(* Comparing trees. *)

let same_fringe (t1 : 'a tree) (t2 : 'a tree) : bool =
  equal (fringe t1) (fringe t2)
