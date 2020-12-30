(* The data values carried by the leaves of a tree. *)

let rec elements t xs =
  match t with
  | Leaf x ->
      x :: xs
  | Fork (t, u) ->
      elements t (elements u xs)

let elements (t : 'a tree) : 'a list =
  elements t []

(* The depths of the leaves of a tree. *)

let rec depths d t ds =
  match t with
  | Leaf _ ->
      d :: ds
  | Fork (t, u) ->
      depths (d + 1) t (depths (d + 1) u ds)

let depths (t : 'a tree) : depth list =
  depths 0 t []

(* The elements and depths, combined. *)

(* Variant: give a direct recursive definition of [spectre]. *)

let rec spectre d t xds =
  match t with
  | Leaf x ->
      (x, d) :: xds
  | Fork (t, u) ->
      spectre (d + 1) t (spectre (d + 1) u xds)

let spectre (t : 'a tree) : 'a spectre =
  spectre 0 t []

(* A facility for reading and consuming the elements of a list. *)

let new_input (xs : 'a list) : 'a input =
  let input, position = ref xs, ref 0 in
  let peek () =
    match !input with [] -> None | x :: _ -> Some x
  and consume () =
    match !input with [] -> assert false | _ :: xs -> input := xs; incr position
  and current () =
    !position
  in
  { peek; consume; current }

(* Reconstructing a tree from its spectre. *)

(* This is inspired by Bird's solution. It has the structure of an LR parser,
   that is, a bottom-up parser. It is a tail-recursive function and uses an
   explicit stack. *)

(* The stack is a list of pairs of a tree and the depth at which this tree
   is supposed to exist. As in an LR parser, the stack represents a part of
   the input that has already been read and processed (reduced). The top
   of the stack is should be thought of as the right end of the stack. *)

(* When [run] or [shift] is invoked, the stack satisfies the following
   property:

   * The (tree, depth) pairs on the stack, read from the left towards the
     right, have strictly increasing [depth] components.

   * The topmost (tree, depth) pair on the stack, if it exists, does not
     have depth zero.

   When [reduce] is invoked, this property is weakened by pushing on top
   of a stack [stack] that satisfies the above property one more tree [u],
   subject to the condition that [can_shift stack u] is true. The stack
   [stack] and the tree [u] are passed to [reduce] as separate arguments. *)

type 'a stack =
  ('a tree * int) list

(* Accessors for pairs of an element (or a tree) and a depth. *)

let data (t, _) =
  t

let depth (_, d) =
  d

(* Well-formedness. *)

let rec sorted (<) xs =
  match xs with
  | [] ->
      true
  | [x] ->
      true
  | x1 :: x2 :: xs ->
      x1 < x2 && sorted (<) (x2 :: xs)

let well_formed stack =
  (* Strictly decreasing depths. *)
  sorted (>=) (List.map snd stack) &&
  (* The topmost stack element has nonzero depth. *)
  match stack with [] -> true | t :: _ -> depth t > 0

(* The constructor [leaf] turns an element with its depth into a leaf with
   its depth. *)

let leaf (u : 'a * int) : 'a tree * int =
  Leaf (data u), depth u

(* The constructor [join] turns two trees [t] and [u] of identical,
   nonzero depth into a [Fork] tree with its depth. *)

let fork (t : 'a tree * int) (u : 'a tree * int) : 'a tree * int =
  let d = depth t in
  assert (d > 0);
  Fork (data t, data u), d - 1

(* [can_shift stack u] determines whether it is permitted to shift the element
   [u] on top of the stack [stack]. If we have [depth t < depth u], then fine.
   If we have [depth t <= depth u], then also fine; the function [reduce] can
   repair the invariant. Otherwise, shifting is not permitted; an ill-formed
   input will be reported. *)

let can_shift stack (u : 'a * int) =
  match stack with
  | [] ->
      true
  | t :: _ ->
      depth t <= depth u

(* The parser consists of three mutually recursive functions [run],
   [shift], and [reduce]. *)

let rec run (stack : 'a stack) (input : ('a * int) input) : 'a tree =
  assert (well_formed stack);
  match input.peek() with
  | None ->
      (* There is no more input. Fail. We cannot be in a final
         state: we would have accepted already. *)
      raise (InputIsTooShort (input.current()))
  | Some u ->
      (* If the next input element [u] can be shifted, then do
         so, otherwise fail. *)
      if can_shift stack u then
        shift stack u input
      else
        raise (InputIsIllFormed (input.current()))

and shift (stack : 'a stack) (u : 'a * int) (input : ('a * int) input) =
  assert (well_formed stack);
  (* Consume the input element [u]. Turn it into a leaf and push
     it onto the stack. Then, call [reduce] to perform zero or
     more reductions and restore the strong invariant that the
     trees on the stack have strictly increasing depths. *)
  input.consume();
  reduce stack (leaf u) input

and reduce (stack : 'a stack) (u : 'a tree * int) (input : ('a * int) input) =
  match stack with
  | _ when depth u = 0 ->
      (* The topmost tree on the stack lies at depth zero.
         The invariant guarantees that the rest of the stack
         must be empty. *)
      assert (stack = []);
      begin match input.peek() with
      | None ->
          data u
      | Some _ ->
          raise (InputIsTooLong (input.current()))
      end
  | t :: stack ->
      assert (depth t <= depth u);
      if depth t = depth u then
        reduce stack (fork t u) input
      else
        run (u :: t :: stack) input
  | [] ->
      run [u] input

let build (depths : 'a spectre) : 'a tree =
  let input = new_input depths in
  run [] input
