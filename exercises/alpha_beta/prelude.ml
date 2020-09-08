(* The type of on-demand sequences. (As of OCaml 4.07, this type exists
   in the standard library.) *)

module Seq = struct

  type 'a t = unit -> 'a node

  and +'a node =
  | Nil
  | Cons of 'a * 'a t

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

(* -------------------------------------------------------------------------- *)

(* [index] turns a list of things into a list of numbered things. *)

let rec index i (xs : 'a list) : (int * 'a) list =
  match xs with
  | [] ->
      []
  | x :: xs ->
      (i, x) :: index (i+1) xs

let index xs =
  index 0 xs

(* The following functions offer facilities for building gamae trees. They may
   appear in the messages produced by the automated grading system. *)

let leaf v =
  TLeaf v

let nonleaf (ts : tree list) : tree =
  TNonLeaf (Seq.of_list (index ts))
    (* [index] is used to generate arbitrary distinct move numbers *)

(* -------------------------------------------------------------------------- *)

(* A notion of sense is used in the reference evaluator, that is, the first
   function that we write in order to compute the value of a game tree. *)

type sense =
  | Even
  | Odd

let opposite sense : sense =
  match sense with Even -> Odd | Odd -> Even

let interpret sense (v : value) : value =
  match sense with Even -> v | Odd -> -v

let join sense : value -> value -> value =
  match sense with Even -> max | Odd -> min

let unit sense : value =
  interpret sense bottom

(* -------------------------------------------------------------------------- *)

(* The following notion of equivalence up to an [alpha, beta] window can be
   used to specify the expected behavior of the Alpha-Beta algorithm. *)

let equivalent alpha beta v1 v2 =
  v1 <= alpha && v2 <= alpha ||
  v1 = v2 ||
  beta <= v1 && beta <= v2
