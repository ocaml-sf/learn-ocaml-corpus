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
    bitmap1 land (bitmap lsr (k1 * direction w h))

let has_alignment w h k bitmap : bool =
  alignments w h k north     bitmap <> 0 ||
  alignments w h k east      bitmap <> 0 ||
  alignments w h k northeast bitmap <> 0 ||
  alignments w h k southeast bitmap <> 0

(* -------------------------------------------------------------------------- *)

let initial w h k : state = {
  w; h; k;
  player_bitmap = 0;
  opponent_bitmap = 0;
  past_moves = 0;
}

let square_is_available state (i, j) : bool =
  let { w; h; player_bitmap; opponent_bitmap } = state in
  i < w && j < h &&
  read w h player_bitmap i j = false &&
  read w h opponent_bitmap i j = false

let play state i j : state =
  let { w; h; k; player_bitmap; opponent_bitmap; past_moves } = state in
  let player_bitmap = update w h player_bitmap i j in
  let player_bitmap, opponent_bitmap = opponent_bitmap, player_bitmap in
  (* let past_moves = past_moves + 1 in *) (* wrong *)
  { w; h; k; player_bitmap; opponent_bitmap; past_moves }

let no_square_is_available state : bool =
  let { w; h; past_moves } = state in
  past_moves = w * h

let opponent_has_won state : bool =
  let { w; h; k; opponent_bitmap } = state in
  has_alignment w h k opponent_bitmap

let player_has_won state : bool =
  let { w; h; k; player_bitmap } = state in
  has_alignment w h k player_bitmap

let rec tree (state : state) : tree =
  (* If the player who is up has lost, then the game is over. *)
  if opponent_has_won state then
    TLeaf (-1) (* we have lost *)
  else begin
    (* If the player who is up has won, then something is wrong;
       the last move (by the other player) should not have been
       played. *)
    assert (not (player_has_won state));
    (* If there are no moves, then the game is a draw. *)
    if no_square_is_available state then
      TLeaf 0
    else
      (* Otherwise, enumerate the permitted moves. *)
      TNonLeaf (offspring state)
  end

and offspring (state : state) : offspring =
  let { w; h } = state in
  Seq.interval 0 ((w + 1) * (h + 1))
  |> Seq.map (decode w h)
  |> Seq.filter (square_is_available state)
  |> Seq.map (fun (i, j) ->
    let state = play state i j in
    let move = encode w h i j in
    (move, tree state)
  )
