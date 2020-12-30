(* Producing on-demand sequences. *)

let fringe (t : 'a tree) : 'a Seq.t =
  fun () -> raise Not_found
