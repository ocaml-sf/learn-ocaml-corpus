(* The type of on-demand sequences. (As of OCaml 4.07, this type exists
   in the standard library.) *)

module Seq = struct

  type 'a t = unit -> 'a node

  and +'a node =
  | Nil
  | Cons of 'a * 'a t

  let nil () =
    Nil

  (* A word of warning: [length] does not terminate if it is applied
     to an infinite sequence. *)

  let rec length accu (xs : 'a t) : int =
    match xs() with
    | Nil ->
        accu
    | Cons (_, xs) ->
        length (accu + 1) xs

  let length xs =
    length 0 xs

  let rec map (f : 'a -> 'b) (xs : 'a t) : 'b t =
    fun () ->
      match xs() with
      | Nil ->
          Nil
      | Cons (x, xs) ->
          Cons (f x, map f xs)

  let rec filter (p : 'a -> bool) (xs : 'a t) : 'a t =
    fun () ->
      match xs() with
      | Nil ->
          Nil
      | Cons (x, xs) ->
          if p x then
            Cons (x, filter p xs)
          else
            filter p xs ()

  let rec assoc (a : 'a) (abs : ('a * 'b) t) : 'b =
    match abs() with
    | Nil ->
        raise Not_found
    | Cons ((a', b), abs) ->
        if a = a' then
          b
        else
          assoc a abs

  let rec of_list (xs : 'a list) : 'a t =
    fun () ->
      match xs with
      | [] ->
          Nil
      | x :: xs ->
          Cons (x, of_list xs)

  let rec to_list (xs : 'a t) : 'a list =
    match xs() with
    | Nil ->
        []
    | Cons (x, xs) ->
        x :: to_list xs

  let rec interval i j : int t =
    if i < j then
      fun () ->
        Cons (i, interval (i+1) j)
    else
      nil

end

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

(* [descendant path tree] extracts the subtree found at path [path] in the
   tree [tree]. A path is a list of moves. The exception [Not_found] is raised
   if this path does not exist in this tree. *)

let rec descendant (path : move list) (tree : tree) : tree =
  match path, tree with
  | [], _ ->
      (* The path is empty. *)
      tree
  | _ :: _, TLeaf _ ->
      (* This path does not exist. *)
      raise Not_found
  | move :: path, TNonLeaf offspring ->
      descendant path (Seq.assoc move offspring)

(* -------------------------------------------------------------------------- *)

(* A bitmap is an OCaml machine word (in other words, an OCaml integer) whose
   bits represent an array of 63 Boolean values. (An OCaml machine word is 63
   bits wide.) *)

type bitmap =
  int

(* An offset is the number of a bit within a machine word. By convention, the
   least significant bit is numbered 0, while the most significant bit is
   numbered 62. *)

type offset =
  int

(* A direction is a function of [w] and [h] to an offset. *)

type direction =
  int -> int -> offset

(* -------------------------------------------------------------------------- *)

(* In a [(w, h, k)] board game, the parameters [w] and [h] are the width and
   height of the board, while the parameter [k] is the number of consecutive
   aligned marks that a player must achieve in order to win. In the standard
   game of Tic-Tac-Toe, all three parameters are equal to 3. *)

(* The parameters [w], [h], [k] remain of course fixed throughout an entire
   game. We nevertheless record them as part of the game state because this
   is convenient. *)

(* The bitmap [player_bitmap] stores the marks of the player who is currently
   up, while the bitmap [opponent_bitmap] stores the marks of his opponent. *)

(* The integer field [past_moves] is the number of moves that have been played
   already. This field is redundant, as its value can be recovered by counting
   how many bits are set in the bitmaps [player_bitmap] and [opponent_bitmap].
   We nevertheless include this field because it is convenient. *)

type state = {
  w: int; h: int; k: int;
  player_bitmap: bitmap;
  opponent_bitmap: bitmap;
  past_moves: int;
}
