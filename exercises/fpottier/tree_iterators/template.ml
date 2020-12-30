(* Producing lists. *)

let rec slow_elements (t : 'a tree) : 'a list =
  (* TO DO: Define this function. *)
  raise TODO

let rec elements_with (t : 'a tree) (ys : 'a list) : 'a list =
  (* TO DO: Define this function. *)
  raise TODO

let elements (t : 'a tree) : 'a list =
  (* TO DO: Define this function. *)
  raise TODO

(* Producing on-demand sequences. *)

let rec fringe_seq_with (t : 'a tree) (ys : 'a Seq.t) : 'a Seq.t =
  (* TO DO: Define this function. *)
  raise TODO

and fringe_node_with (t : 'a tree) (ys : 'a Seq.t) : 'a Seq.node =
  (* TO DO: Define this function. *)
  raise TODO

let fringe (t : 'a tree) : 'a Seq.t =
  (* TO DO: Define this function. *)
  raise TODO

(* Comparing sequences. *)

let rec equal (xs : 'a Seq.t) (ys : 'a Seq.t) : bool =
  (* TO DO: Define this function. *)
  raise TODO

(* Comparing trees. *)

let same_fringe (t1 : 'a tree) (t2 : 'a tree) : bool =
  (* TO DO: Define this function. *)
  raise TODO
