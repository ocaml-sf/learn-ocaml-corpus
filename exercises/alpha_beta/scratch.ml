open Seq

(* -------------------------------------------------------------------------- *)

(* A value is an integer value that reflects the current player view (or
   evaluation) of the current game configuration. In a zero-sum game, the
   opponent's view is the opposite of the current player's view, and the
   value 0 means that both players are even; in particular, in a situation
   where no more moves can be played, the value 0 represents a draw. *)

type value =
  int

(* All of the values that we consider lie in the range [bottom] to [top],
   inclusive. Note that [bottom] is the opposite of [top]. *)

let bottom, top =
  -max_int, max_int

(* We assume that, in every game configuration, a finite number of moves
   are permitted, and that each of these moves can be identified with an
   integer code. (A more realistic implementation would be parametric in
   the type [move].) *)

type move =
  int

(* A game tree is either a leaf or not a leaf. A leaf [TLeaf v] means that the
   game is over and that its value (in the eyes of the current player) is [v].
   A nonleaf [TNonLeaf mts] means that the game is not over. In that case, at
   least one move is permitted. The sequence [mts] is then a nonempty sequence
   of pairs of a permitted move [m] and the subtree that corresponds to this
   move. This sequence covers all of the permitted moves. *)

type tree =
  | TLeaf of value
  | TNonLeaf of offspring

and offspring =
  (move * tree) Seq.t

(* -------------------------------------------------------------------------- *)

(* The board representation. *)

(* A 3x3 grid is represented as follows:

   .  .  . .
   2  6 10 .
   1  5  9 .
   0  4  8 .

   We leave an empty row at the top and an empty column at the right-hand
   side, so as to facilitate the check for the existence of an alignment.

   As long as `(w + 1) * (h + 1)` is less than or equal to `Sys.word_size`,
   a bitmap fits in an OCaml machine integer.

   We use two bitmaps, one per player, which we store in two separate
   integer words, for simplicity. *)

(* The four directions, expressed as positive bitmap offsets. *)

let north w h =
  1

let east w h =
  h + 1

let southeast w h =
  h

let northeast w h =
  h + 2

(* [index i j] is the bit index associated with a position [i, j] on the board.
   [i] is the line number and [j] is the column number. *)

let index w h i j =
  assert (0 <= i && i < h);
  assert (0 <= j && j < w);
  (h + 1) * j + i
  (* TEMPORARY exchange i and j *)

let mask w h i j =
  1 lsl (index w h i j)

let read w h bitmap i j : bool =
  bitmap land (mask w h i j) <> 0

let update w h bitmap i j : int =
  bitmap lor (mask w h i j)

(* Predicates on alignments. *)

(* In our representations of bitmaps, index i+north lies vertically above
   index i. So, (bitmap & (bitmap >> north)) is the set of those indices i
   such that both i and i+north are set in bitmap. (We needn't worry about
   overflows at the top of the board, thanks to the empty row on top.)
   By iterating this idea (a logarithmic number of times!) we get a set of
   the indices i such that we have a northward alignment of k set bits at
   index i. This idea can be generalized to any direction; the empty row
   on top and the empty column on the right-hand side suffice to take care
   of any potential issues at the borders. *)

(* [aligments k direction bitmap] returns a bitmap of the starting positions
   of all [k]-alignments along [direction] in [bitmap]. *)

let rec alignments k direction bitmap =
  (* Base case: if [k] is 1, there is nothing to compute. *)
  if k = 1 then
    bitmap
  (* We inline the case where [k] is 2, for speed. *)
  else if k = 2 then
    bitmap land (bitmap lsr direction)
  (* When [k] is even, one recursive call suffices. *)
  else if k mod 2 = 0 then
    let k2 = k / 2 in
    let bitmap2 = alignments k2 direction bitmap in
    bitmap2 land (bitmap2 lsr (k2 * direction))
  (* The general case. We compute all [k1]-alignments and
     all [k2]-alignments, where [k1 + k2 = k]. Then, we
     intersect the results. In fact, we pick [k2 = 1] so
     as to perform just one recursive call. *)
  else
    let k1 = k - 1 in
    let bitmap1 = alignments k1 direction bitmap in
    bitmap1 land (bitmap lsr (k1 * direction))

(* This checks whether there exists a [k]-alignment along any direction. *)

let is_winning w h k bitmap =
  alignments k (north w h)     bitmap != 0 ||
  alignments k (east w h)      bitmap != 0 ||
  alignments k (southeast w h) bitmap != 0 ||
  alignments k (northeast w h) bitmap != 0

(* -------------------------------------------------------------------------- *)

(* Building a game tree for Tic-Tac-Toe. *)

let nil () =
  Seq.Nil

let rec interval i j : int Seq.t =
  if i < j then
    fun () ->
      Seq.Cons (i, interval (i+1) j)
  else
    nil

type state = {
  w: int; h: int; k: int;
  player_bitmap: int;
  opponent_bitmap: int;
  past_moves: int;
}

let initial w h k : state = {
  w; h; k;
  player_bitmap = 0;
  opponent_bitmap = 0;
  past_moves = 0;
}

let available state i j : bool =
  let { w; h; player_bitmap; opponent_bitmap } = state in
  read w h player_bitmap i j = false &&
  read w h opponent_bitmap i j = false

let play state i j =
  let { w; h; k; player_bitmap; opponent_bitmap; past_moves } = state in
  let player_bitmap = update w h player_bitmap i j in
  let player_bitmap, opponent_bitmap = opponent_bitmap, player_bitmap in
  let past_moves = past_moves + 1 in
  { w; h; k; player_bitmap; opponent_bitmap; past_moves }

let opponent_has_won state =
  let { w; h; k; opponent_bitmap } = state in
  is_winning w h k opponent_bitmap

let player_has_won state =
  let { w; h; k; player_bitmap } = state in
  is_winning w h k player_bitmap

let no_square_is_available state =
  let { w; h; past_moves } = state in
  past_moves = w * h

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
      TNonLeaf (moves state)
  end

and moves (state : state) : offspring =
  let { w; h } = state in
  interval 0 ((w + 1) * (h + 1))
  |> Seq.map (fun ij -> ij mod (h + 1), ij / (h + 1))
  |> Seq.filter (fun (i, j) -> i < h && j < w && available state i j)
  |> Seq.map (fun (i, j) ->
    let state = play state i j in
    let move = index w h i j in
    (move, tree state)
  )

(* -------------------------------------------------------------------------- *)

(* TEMPORARY

(* Building a game tree for Tic-Tac-Toe. *)

(* The players are named X and O. X plays first. *)

type entry =
  | Empty
  | X
  | O

(* The board is represented (in a simple-minded manner) as a matrix
   of entries, whose dimensions are [width] and [height]. This matrix
   is regarded as immutable. *)

type state = {
  width: int;
  height: int;
  board: entry array;
  goal: int;
  past_moves: int;
}

let initial width height goal : state = {
  width;
  height;
  board = Array.make (width * height) Empty;
  goal;
  past_moves = 0
}

let index state i j =
  assert (0 <= i && i < state.width);
  assert (0 <= j && j < state.height);
  state.height * i + j

let get state i j : entry =
  state.board.(index state i j)

let set state i j player : state =
  assert (get state i j = Empty);
  let board = Array.copy state.board in
  board.(index state i j) <- player;
  let past_moves = state.past_moves + 1 in
  { state with board; past_moves }

let rec alignment state entry i j di dj goal : bool =
  goal = 0 ||
  get state i j = entry && alignment state entry (i+di) (j+dj) di dj (goal-1)

let horizontal_alignment state entry i j : bool =
  alignment state entry i j 1 0 state.goal

let vertical_alignment state entry i j : bool =
  alignment state entry i j 0 1 state.goal

let northeast_alignment state entry i j : bool =
  alignment state entry i j 1 1 state.goal

let southeast_alignment state entry i j : bool =
  alignment state entry i j 1 (-1) state.goal

let rec exists i j f =
  i < j && (f i || exists (i+1) j f)

let any_alignment state entry : bool =
  (* horizontal *)
  exists 0 (state.width - state.goal + 1) (fun i ->
    exists 0 state.height (fun j ->
      horizontal_alignment state entry i j
    )
  ) ||
  (* vertical *)
  exists 0 state.width (fun i ->
    exists 0 (state.height - state.goal + 1) (fun j ->
      vertical_alignment state entry i j
    )
  ) ||
  (* northeast *)
  exists 0 (state.width - state.goal + 1) (fun i ->
    exists 0 (state.height - state.goal + 1) (fun j ->
      northeast_alignment state entry i j
    )
  ) ||
  (* southeast *)
  exists 0 (state.width - state.goal + 1) (fun i ->
    exists (state.goal - 1) state.height (fun j ->
      southeast_alignment state entry i j
    )
  )

let rec tree (state : state) : tree =
  (* If the player who is up has lost, then the game is over. *)
  let opponent = if state.past_moves mod 2 = 0 then O else X in
  if any_alignment state opponent then
    TLeaf (-1) (* we have lost *)
  else begin
    (* If the player who is up has won, then something is wrong;
       the last move (by the other player) should not have been
       played. *)
    let player = if state.past_moves mod 2 = 0 then X else O in
    assert (not (any_alignment state player));
    (* If there are no moves, then the game is a draw. *)
    if state.past_moves = state.width * state.height then
      TLeaf 0
    else
      (* Otherwise, enumerate the permitted moves. *)
      TNonLeaf (moves state player)
  end

and moves state player =
  fold_interval 0 state.width (fun i accu ->
    fold_interval 0 state.height (fun j accu ->
      if available state i j then
        fun () ->
          Seq.Cons (play state player i j, accu)
      else
        accu
    ) accu
  ) nil

(* pas content ici de l'appel recursif a tree *)
and play state player i j =
  let move = index state i j in
  let state = set state i j player in
  (move, tree state)

*)
