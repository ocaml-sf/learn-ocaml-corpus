(* Basic constructor functions. *)

(* BEGIN INCLUDE *)
(* TO DO: Define [empty]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
let empty : 'a enum =
  fun _s ->
    Seq.empty
     END EXCLUDE *)

let just (x : 'a) : 'a enum =
(* BEGIN INCLUDE *)
  (* TO DO: Complete this definition. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  fun s ->
    if s = 0 then Seq.singleton x else Seq.empty
     END EXCLUDE *)

let pay (enum : 'a enum) : 'a enum =
(* BEGIN INCLUDE *)
  (* TO DO: Complete this definition. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  fun s ->
    (* Treating [s = 0] as a special case is required, because we have asked
       the student to guarantee that every size argument [s] ever passed to
       an enumeration is nonnegative. *)
    if s = 0 then Seq.empty else enum (s-1)
     END EXCLUDE *)

let sum (enum1 : 'a enum) (enum2 : 'a enum) : 'a enum =
(* BEGIN INCLUDE *)
  (* TO DO: Complete this definition. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  fun s ->
    Seq.sum (enum1 s) (enum2 s)
     END EXCLUDE *)

let ( ++ ) =
  sum

(* BEGIN EXCLUDE
let rec up i j =
  if i <= j then
    i :: up (i + 1) j
  else
    []

     END EXCLUDE *)
let product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
(* BEGIN INCLUDE *)
  (* TO DO: Complete this definition. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  fun s ->
    Seq.bigsum (
      List.map (fun s1 ->
        let s2 = s - s1 in
        Seq.product (enum1 s1) (enum2 s2)
      ) (up 0 s)
    )
     END EXCLUDE *)

let ( ** ) =
  product

let map (phi : 'a -> 'b) (enum : 'a enum) : 'b enum =
(* BEGIN INCLUDE *)
  (* TO DO: Complete this definition. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  fun s ->
    Seq.map phi (enum s)
     END EXCLUDE *)

(* Derived constructor functions. *)

(* BEGIN INCLUDE *)
(* TO DO: Define [bit]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
let bit : int enum =
  just 0 ++ just 1
     END EXCLUDE *)

let list (elem : 'a enum) : 'a list enum =
(* BEGIN INCLUDE *)
  (* TO DO: Complete this definition. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  let cons (x, xs) = x :: xs in
  fix (fun list ->
    just [] ++ pay (map cons (elem ** list))
  )
     END EXCLUDE *)

(* BEGIN INCLUDE *)
(* TO DO: Define [tree]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
let tree : tree enum =
  let node (t1, t2) = Node (t1, t2) in
  fix (fun tree ->
    just Leaf ++ pay (map node (tree ** tree))
  )
     END EXCLUDE *)

let balanced_product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
(* BEGIN INCLUDE *)
  (* TO DO: Complete this definition. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  fun s ->
    if s mod 2 = 0 then
      let s = s / 2 in
      Seq.product (enum1 s) (enum2 s)
    else
      let s = s / 2 in
      Seq.sum
        (Seq.product (enum1 s) (enum2 (s+1)))
        (Seq.product (enum1 (s+1)) (enum2 s))
     END EXCLUDE *)

let ( *-* ) =
  balanced_product

(* BEGIN INCLUDE *)
(* TO DO: Define [balanced_tree]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
let balanced_tree : tree enum =
  let node (t1, t2) = Node (t1, t2) in
  fix (fun tree ->
    just Leaf ++ pay (map node (tree *-* tree))
  )
     END EXCLUDE *)

(* BEGIN INCLUDE *)
(* TO DO: Define [balanced_tidy_tree]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
let postincr c =
  let x = !c in
  c := x + 1;
  x

let label (t : tree) : labeled_tree =
  let c = ref 0 in
  let rec label t =
    match t with
    | Leaf ->
        LLeaf (postincr c)
    | Node (t1, t2) ->
        let t1 = label t1 in
        let t2 = label t2 in
        LNode (t1, t2)
  in
  label t

let balanced_tidy_tree : labeled_tree enum =
  map label balanced_tree
     END EXCLUDE *)
