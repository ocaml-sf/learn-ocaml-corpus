(* Addition of requirements. *)

let (++) (x : req) (y : req) : req =
  match x, y with
  | Infinite, _
  | _, Infinite ->
      Infinite
  | Finite x, Finite y ->
      Finite (x + y)

(* Comparison of requirements. *)

let (<==) (x : req) (y : req) : bool =
  match x, y with
  | _, Infinite ->
      true
  | Infinite, Finite _ ->
      false
  | Finite x, Finite y ->
      x <= y

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
  | IfFlat (doc, _) ->
      requirement doc
  | Cat (req, _, _)
  | Nest (_, req, _)
  | Group (req, _) ->
      req

(* Smart constructors. *)

let empty : doc =
  Empty

let hardline : doc =
  HardLine

let char (c : char) : doc =
  Char c (* wrong *)

let ifflat (doc1 : doc) (doc2 : doc) : doc =
  (* Mandatory: check whether [doc1] is [IfFlat _], so as to avoid
     nesting [IfFlat] in the left-hand side of [IfFlat]. *)
  match doc1 with
  | IfFlat (doc1, _)
  | doc1 ->
      IfFlat (doc1, doc2)

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

let rec pretty state (indent : int) (flatten : bool) doc =
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

  | IfFlat (doc1, doc2) ->
      pretty state indent flatten (if flatten then doc1 else doc2)

  | Cat (_, doc1, doc2) ->
      pretty state indent flatten doc1;
      pretty state indent flatten doc2

  | Nest (i, _, doc) ->
      pretty state (indent + i) flatten doc

  | Group (req, doc) ->
      let flatten =
        flatten || Finite state.column ++ req <== Finite state.width
      in
      pretty state indent flatten doc


let pretty width doc =
  let output = Buffer.create 512 in
  let state = { width; column = 0; output } in
  pretty state 0 false doc;
  Buffer.contents output

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
