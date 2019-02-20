(* Basic constructor functions. *)

let empty : 'a enum =
  fun s ->
    Seq.empty

let just (x : 'a) : 'a enum =
  fun s ->
    if s = 0 then Seq.singleton x else Seq.empty

let pay (enum : 'a enum) : 'a enum =
  fun s ->
    enum (s-1) (* wrong: forget to test if [s] is zero *)
