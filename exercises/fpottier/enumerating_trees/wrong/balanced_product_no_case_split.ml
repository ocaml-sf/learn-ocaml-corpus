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

let rec up i j =
  if i <= j then
    i :: up (i + 1) j
  else
    []

let product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
  fun s ->
    Seq.bigsum (
      List.map (fun s1 ->
        let s2 = s - s1 in
        Seq.product (enum1 s1) (enum2 s2)
      ) (up 0 s)
    )

let ( ** ) =
  product

let map (phi : 'a -> 'b) (enum : 'a enum) : 'b enum =
  fun s ->
    Seq.map phi (enum s)

(* Derived constructor functions. *)

let bit : int enum =
  just 0 ++ just 1

let list (elem : 'a enum) : 'a list enum =
  let cons (x, xs) = x :: xs in
  fix (fun list ->
    just [] ++ pay (map cons (elem ** list))
  )

let tree : tree enum =
  let node (t1, t2) = Node (t1, t2) in
  fix (fun tree ->
    just Leaf ++ pay (map node (tree ** tree))
  )

let balanced_product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
  fun s ->
    let s1 = s / 2 in
    let s2 = s - s1 in
    Seq.product (enum1 s1) (enum2 s2)
    (* wrong: missing some possibilities *)
