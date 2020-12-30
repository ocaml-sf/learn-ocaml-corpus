(* Addition of requirements. *)

let (++) (req1 : req) (req2 : req) : req =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  match req1, req2 with
  | Infinite, _
  | _, Infinite ->
      Infinite
  | Finite i1, Finite i2 ->
      Finite (i1 + i2)
(*   END EXCLUDE *)

(* Comparison of requirements. *)

let (<==) (req1 : req) (req2 : req) : bool =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  match req1, req2 with
  | _, Infinite ->
      true
  | Infinite, Finite _ ->
      false
  | Finite i1, Finite i2 ->
      i1 <= i2
(*   END EXCLUDE *)

(* Determining the space requirement of a document. *)

(* This function is expected to run in constant time. *)

let rec requirement (doc : doc) : req =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
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
(*   END EXCLUDE *)

(* Smart constructors. *)

let empty : doc =
  Empty

let hardline : doc =
  HardLine

let char (c : char) : doc =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  if c = '\n' then HardLine else Char c
(*   END EXCLUDE *)

let (^^) (doc1 : doc) (doc2 : doc) : doc =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  (* Optional: treat [empty] as a neutral element for concatenation. *)
  match doc1, doc2 with
  | Empty, doc
  | doc, Empty ->
      doc
  | _, _ ->
      Cat (requirement doc1 ++ requirement doc2, doc1, doc2)
(*   END EXCLUDE *)

let nest (i : int) (doc : doc) : doc =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  assert (i >= 0);
  (* Optional: recognize that two [Nest] constructors can be combined. *)
  match doc with
  | Nest (j, req, doc) ->
      Nest (i + j, req, doc)
  | _ ->
      Nest (i, requirement doc, doc)
(*   END EXCLUDE *)

let group (doc : doc) : doc =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  match doc with
  | Group _ ->
      (* Optional: recognize that two [Group] constructors can be combined. *)
      doc
  | _ ->
      let req = requirement doc in
      Group (req, doc)
(*   END EXCLUDE *)

let ifflat (doc1 : doc) (doc2 : doc) : doc =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  (* Mandatory: check whether [doc1] is [IfFlat _], so as to avoid
     nesting [IfFlat] in the left-hand side of [IfFlat]. *)
  match doc1 with
  | IfFlat (doc1, _)
  | doc1 ->
      IfFlat (doc1, doc2)
(*   END EXCLUDE *)

let rec render state (indent : int) (flatten : bool) doc =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  match doc with

  | Empty ->
      ()

  | HardLine ->
      assert (not flatten);
      Buffer.add_char state.output '\n';
      for i = 1 to indent do
        Buffer.add_char state.output ' '
      done;
      state.column <- indent

  | Char c ->
      Buffer.add_char state.output c;
      state.column <- state.column + 1

  | Cat (_, doc1, doc2) ->
      render state indent flatten doc1;
      render state indent flatten doc2

  | Nest (i, _, doc) ->
      render state (indent + i) flatten doc

  | Group (req, doc) ->
      let flatten =
        flatten || Finite state.column ++ req <== Finite state.width
      in
      render state indent flatten doc

  | IfFlat (doc1, doc2) ->
      render state indent flatten (if flatten then doc1 else doc2)
(*   END EXCLUDE *)

let pretty width doc =
(* BEGIN INCLUDE
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  let output = Buffer.create 512 in
  let state = { width; column = 0; output } in
  render state 0 false doc;
  Buffer.contents output
(*   END EXCLUDE *)

(* BEGIN EXCLUDE *)
(* The following combinators must be defined using the smart constructors. *)

let concat (docs : doc list) : doc =
  List.fold_right (^^) docs empty

let chars (s : string) : char list =
  let rec loop i =
    if i < String.length s then s.[i] :: loop (i + 1) else []
  in
  loop 0

(* A high-level definition of [string]. *)

let string (s : string) : doc =
  s |> chars |> List.map char |> concat

(* A direct, low-level definition of [string]. *)

let string (s : string) : doc =
  let rec loop i =
    if i < String.length s then char s.[i] ^^ loop (i + 1) else empty
  in
  loop 0

(*   END EXCLUDE *)
