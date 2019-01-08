let empty : heap =
  E

(* BEGIN INCLUDE
(* TO DO: Define the auxiliary function [rank]. *)
let rank (h : heap) : rank =
  assert false
     END INCLUDE *)
(* BEGIN EXCLUDE *)
let rank (h : heap) : rank =
  match h with
  | E ->
      0
  | T (r, _, _, _) ->
      r
(*   END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: Define the auxiliary function [makeT]. *)
let makeT (x : element) (h1 : heap) (h2 : heap) : heap =
  assert false
     END INCLUDE *)
(* BEGIN EXCLUDE *)
let makeT x h1 h2 =
  let r1 = rank h1
  and r2 = rank h2 in
  if r1 >= r2 then
    T (r2 + 1, x, h1, h2)
  else
    T (r1 + 1, x, h2, h1)
(*   END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: Define [singleton]. *)
let singleton (x : element) : heap =
  assert false
     END INCLUDE *)
(* BEGIN EXCLUDE *)
let singleton (x : element) : heap =
  makeT x empty empty
  (* T(1, x, E, E) *)
(*   END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: Define [union]. *)
let union (h1 : heap) (h2 : heap) : heap =
  assert false
     END INCLUDE *)
(* BEGIN EXCLUDE *)
let rec union h1 h2 =
  match h1, h2 with
  | E, h
  | h, E ->
      h
  | T (_, x1, a1, b1), T (_, x2, a2, b2) ->
      if priority x1 <= priority x2 then
        makeT x1 a1 (union b1 h2)
      else
        makeT x2 a2 (union h1 b2)
(*   END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: Define [insert]. *)
let insert (x : element) (h : heap) : heap =
  assert false
     END INCLUDE *)
(* BEGIN EXCLUDE *)
let insert (x : element) (h : heap) : heap =
  union (singleton x) h
(*   END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: Define [extract]. *)
let extract (h : heap) : (element * heap) option =
  assert false
     END INCLUDE *)
(* BEGIN EXCLUDE *)
let extract (h : heap) : (element * heap) option =
  match h with
  | E ->
      None
  | T (_, x, h1, h2) ->
      Some (x, union h1 h2)
(*   END EXCLUDE *)
