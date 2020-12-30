(* -------------------------------------------------------------------------- *)

(* Accessing and updating a bitmap, one bit at a time. *)

let encode w h i j : offset =
  assert (0 <= i && i <= w);
  assert (0 <= j && j <= h);
  (h + 1) * i + j

let decode w h (o : offset) : int * int =
  o / (h + 1),
  o mod (h + 1)

let mask w h i j : bitmap =
  1 lsl (encode w h i j)

let read w h (bitmap : bitmap) i j : bool =
  bitmap land (mask w h i j) <> 0

let update w h (bitmap : bitmap) i j : bitmap =
  bitmap lor (mask w h i j)

(* -------------------------------------------------------------------------- *)

(* Finding out whether a bitmap contains an alignment of [k] marks. *)

let north w h : offset =
  1

let east w h : offset =
  h (* wrong *)

let northeast w h : offset =
  h + 2
  (* also: east w h + north w h *)

let southeast w h : offset =
  h
  (* also: east w h - north w h *)
