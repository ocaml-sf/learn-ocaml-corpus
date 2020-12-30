exception TODO

(* The following code is to used to define the function [bitmap], which
   is not used by the automatic grader, but can appear in the messages
   produced by the automatic grader, and can be used by the student. *)

(* Our definition is not as efficient as it could be, but is reasonably
   simple. Speed does not really matter here. *)

module Bitmap = struct

  (* The integers of [i] (included) to [j] (excluded). *)

  let rec up i j =
    if i < j then
      i :: up (i + 1) j
    else
     []

  (* The same, counting down. *)

  let down i j =
    List.rev (up i j)

  (* Cartesian product of two lists. *)

  let cartesian_product xs ys f =
    List.fold_right (fun x accu ->
      List.fold_right (fun y accu ->
        f x y :: accu
      ) ys accu
    ) xs []

  (* Converting a string to a list. *)

  let string_to_list (s : string) : char list =
    let n = String.length s in
    let rec aux i = if i = n then [] else s.[i] :: aux (i+1) in
    aux 0

  (* The coordinates in a matrix, presented in the order where
     they are printed, i.e., top row first, then second row,
     and so on. *)

  let coordinates w h =
    cartesian_product (down 0 (h+1)) (up 0 (w+1)) (fun j i -> (i, j))

  (* [encode], [mask], [update], as in the solution. *)

  let encode w h i j : offset =
    assert (0 <= i && i <= w);
    assert (0 <= j && j <= h);
    (h + 1) * i + j

  let mask w h i j : bitmap =
    1 lsl (encode w h i j)

  let update w h (bitmap : bitmap) i j : bitmap =
    bitmap lor (mask w h i j)

  (* Filtering out whitespace characters. *)

  let is_not_whitespace = function
    | ' '
    | '\n'
    | '\r'
    | '\t' ->
        false
    | _ ->
        true

  (* [bitmap] converts a string to a bitmap. We read the characters in the
     order they are presented to us. Each character is '0' or '1'. At the same
     time, we read the list [coordinates], which has the same length, and
     contains pairs (i, j). We update the bitmap as we go. *)

  let bitmap w h (s : string) : bitmap =
    let n = (w + 1) * (h + 1) in
    let chars = string_to_list s |> List.filter is_not_whitespace in
    if List.length chars <> n then
      invalid_arg "bitmap";
    let coordinates = coordinates w h in
    assert (List.length coordinates = n);
    List.fold_left2 (fun bitmap c (i, j) ->
      match c with
      | '0'
      | '.' -> bitmap
      | '1' -> update w h bitmap i j
      | _   -> invalid_arg "bitmap"
    ) 0 chars coordinates

end

let bitmap =
  Bitmap.bitmap
