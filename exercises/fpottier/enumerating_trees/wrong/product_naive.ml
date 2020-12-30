(* Basic constructor functions. *)

let empty : 'a enum =
  fun _s ->
    Seq.empty

let just (x : 'a) : 'a enum =
  fun s ->
    if s = 0 then Seq.singleton x else Seq.empty

let pay (enum : 'a enum) : 'a enum =
  fun s ->
    (* Treating [s = 0] as a special case is required, because we have asked
       the student to guarantee that every size argument [s] ever passed to
       an enumeration is nonnegative. *)
    if s = 0 then Seq.empty else enum (s-1)

let sum (enum1 : 'a enum) (enum2 : 'a enum) : 'a enum =
  fun s ->
    Seq.sum (enum1 s) (enum2 s)

let ( ++ ) =
  sum

let product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
  fun s ->
    Seq.product (enum1 s) (enum2 s)
