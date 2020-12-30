(* Basic constructor functions. *)

let empty : 'a enum =
  fun _s ->
    Seq.empty

let just (x : 'a) : 'a enum =
  assert false
    (* fail early, without waiting for [s] *)
