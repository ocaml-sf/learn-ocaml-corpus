(* The data values carried by the leaves of a tree. *)

(* BEGIN EXCLUDE
let rec elements t xs =
  match t with
  | Leaf x ->
      x :: xs
  | Fork (t, u) ->
      elements t (elements u xs)

     END EXCLUDE *)
let elements (t : 'a tree) : 'a list =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  elements t []
     END EXCLUDE *)

(* The depths of the leaves of a tree. *)

(* BEGIN EXCLUDE
let rec depths d t ds =
  match t with
  | Leaf _ ->
      d :: ds
  | Fork (t, u) ->
      depths (d + 1) t (depths (d + 1) u ds)

     END EXCLUDE *)
let depths (t : 'a tree) : depth list =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  depths 0 t []
     END EXCLUDE *)

(* The elements and depths, combined. *)

let spectre (t : 'a tree) : 'a spectre =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  List.combine (elements t) (depths t)
    (* A direct definition can also be given. *)
     END EXCLUDE *)

(* A facility for reading and consuming the elements of a list. *)

let new_input (xs : 'a list) : 'a input =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  let input, position = ref xs, ref 0 in
  let peek () =
    match !input with [] -> None | x :: _ -> Some x
  and consume () =
    match !input with [] -> assert false | _ :: xs -> input := xs; incr position
  and current () =
    !position
  in
  { peek; consume; current }
     END EXCLUDE *)

(* Reconstructing a tree from its spectre. *)

(* BEGIN EXCLUDE
(* This is Filliâtre's solution, which he attributes to Tarjan. It has the
   structure of an LL parser, that is, a recursive descent parser. It is a
   recursive function and therefore uses an implicit stack. *)

let rec subtree (depth : int) (input : ('a * int) input) : 'a tree =
  match input.peek() with
  | None ->
      (* Premature end of input. *)
      raise (InputIsTooShort (input.current()))
  | Some (x, d) ->
      (* If this element lies at our expected depth [depth], then we must
         consume it and make it a [Leaf]. *)
      if d = depth then begin
        input.consume();
        Leaf x
      end
      (* If this element lies further down than our expected depth, then it
         must be part of a subtree that we have not yet built, and which has
         a [Fork] at its root. So, we do not consume this element. Instead,
         we perform two recursive calls (on our unchanged input) that build
         two subtrees [t1] and [t2] at expected depth [d+1], and we combine
         them into a single subtree. *)
      else if d > depth then
        let t1 = subtree (depth + 1) input in
        let t2 = subtree (depth + 1) input in
        Fork (t1, t2)
      else
        (* We have [d < depth]. This element lies higher than our expected
           depth. The input is ill-formed. *)
        raise (InputIsIllFormed (input.current()))

     END EXCLUDE *)
let build (depths : 'a spectre) : 'a tree =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  let input = new_input depths in
  let t = subtree 0 input in
  match input.peek() with
  | None ->
      t
  | Some _ ->
      raise (InputIsTooLong (input.current()))

(* Note: Bird's solution, which has the structure of a bottom-up (LR) parser,
   is given in the file right/LR.ml. *)
     END EXCLUDE *)
