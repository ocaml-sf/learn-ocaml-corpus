(* Addition of requirements. *)

let (++) (req1 : req) (req2 : req) : req =
  match req1, req2 with
  | Infinite, _
  | _, Infinite ->
      Infinite
  | Finite i1, Finite i2 ->
      Finite (i1 + i2)

(* Comparison of requirements. *)

let (<==) (req1 : req) (req2 : req) : bool =
  match req1, req2 with
  | _, Infinite ->
      true
  | Infinite, Finite _ ->
      false
  | Finite i1, Finite i2 ->
      i1 <= i2

(* Determining the space requirement of a document. *)

(* This function is expected to run in constant time. *)

let rec requirement (doc : doc) : req =
  match doc with
  | Empty ->
      Finite 0
  | HardLine ->
      Infinite
  | Char _ ->
      Finite 1
  | Cat (req, _, _)
  | Nest (_, req, _)
  | Group (req, _) ->
      req
  | IfFlat (doc, _) ->
      requirement doc

(* Smart constructors. *)

let empty : doc =
  Empty

let hardline : doc =
  HardLine

let char (c : char) : doc =
  if c = '\n' then HardLine else Char c

let (^^) (doc1 : doc) (doc2 : doc) : doc =
  (* Optional: treat [empty] as a neutral element for concatenation. *)
  match doc1, doc2 with
  | Empty, doc
  | doc, Empty ->
      doc
  | _, _ ->
      Cat (requirement doc1 ++ requirement doc2, doc1, doc2)

let nest (i : int) (doc : doc) : doc =
  assert (i >= 0);
  (* Optional: recognize that two [Nest] constructors can be combined. *)
  match doc with
  | Nest (j, req, doc) ->
      Nest (i + j, req, doc)
  | _ ->
      Nest (i, requirement doc, doc)

let group (doc : doc) : doc =
  match doc with
  | Group _ ->
      (* Optional: recognize that two [Group] constructors can be combined. *)
      doc
  | _ ->
      let req = requirement doc in
      Group (req, doc)

(* Gabriel's smart [ifflat]. This function must be ACCEPTED. *)

let ifflat (doc1 : doc) (doc2 : doc) : doc =
  let flattening = function
    | IfFlat (flat, _notflat) -> flat
    | doc -> doc
  in
  let not_flattening = function
    | IfFlat (_flat, notflat) -> notflat
    | doc -> doc
  in
  IfFlat (flattening doc1, not_flattening doc2)

(* This test is REJECTED only because [pretty] is missing.
   It is actually a positive test in disguise. *)
