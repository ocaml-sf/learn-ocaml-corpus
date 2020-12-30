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
  h + 1

let northeast w h : offset =
  h + 2
  (* also: east w h + north w h *)

let southeast w h : offset =
  h
  (* also: east w h - north w h *)

let rec alignments w h k direction bitmap : bitmap =
  (* Base case: if [k] is 1, there is nothing to compute. *)
  if k = 1 then
    bitmap
  (* We inline the case where [k] is 2, for speed. *)
  else if k = 2 then
    bitmap land (bitmap lsr direction w h)
  (* When [k] is even, one recursive call suffices. *)
  else if k mod 2 = 0 then
    let k2 = k / 2 in
    let bitmap2 = alignments w h k2 direction bitmap in
    bitmap2 land (bitmap2 lsr (k2 * direction w h))
  (* The general case. We compute all [k1]-alignments and
     all [k2]-alignments, where [k1 + k2 = k]. Then, we
     intersect the results. In fact, we pick [k2 = 1] so
     as to perform just one recursive call. *)
  else
    let k1 = k - 1 in
    let bitmap1 = alignments w h k1 direction bitmap in
    bitmap1 land (bitmap lsr ((* k1 * wrong *) direction w h))

let has_alignment w h k bitmap : bool =
  alignments w h k north     bitmap <> 0 ||
  alignments w h k east      bitmap <> 0 ||
  alignments w h k northeast bitmap <> 0 ||
  alignments w h k southeast bitmap <> 0
