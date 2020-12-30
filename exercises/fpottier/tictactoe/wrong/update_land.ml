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
  bitmap land (* wrong *) (mask w h i j)
