(* Basic constructor functions. *)

let empty : 'a enum =
  fun _s ->
    Seq.empty

let just (x : 'a) : 'a enum =
  fun s ->
    if s = 0 then Seq.singleton x else Seq.empty

let pay (enum : 'a enum) : 'a enum =
  fun s ->
    if s = 0 then Seq.empty else enum (s+1) (* wrong: using plus instead of minus! *)
