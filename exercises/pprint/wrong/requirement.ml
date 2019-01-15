let (++) (x : req) (y : req) : req =
  match x, y with
  | Infinite, _
  | _, Infinite ->
      Infinite
  | Finite x, Finite y ->
      Finite (x + y)

let (<==) (x : req) (y : req) : bool =
  match x, y with
  | _, Infinite ->
      true
  | Infinite, Finite _ ->
      false
  | Finite x, Finite y ->
      x <= y

let max x y =
  match x, y with
  | _, Infinite
  | Infinite, _ ->
      Infinite
  | Finite x, Finite y ->
      Finite (max x y)

let rec requirement (doc : doc) : req =
  match doc with
  | Empty ->
      Finite 0
  | HardLine ->
      Infinite
  | Char _ ->
      Finite 1
  | IfFlat (doc1, doc2) ->
      max (requirement doc1) (requirement doc2) (* wrong *)
  | Cat (req, _, _)
  | Nest (_, req, _)
  | Group (req, _) ->
      req

let empty : doc =
  Empty

let hardline : doc =
  HardLine

let char (c : char) : doc =
  if c = '\n' then HardLine else Char c

let ifflat (doc1 : doc) (doc2 : doc) : doc =
  match doc1 with
  | IfFlat (doc1, _)
  | doc1 ->
      IfFlat (doc1, doc2)

let (^^) (doc1 : doc) (doc2 : doc) : doc =
  match doc1, doc2 with
  | Empty, doc
  | doc, Empty ->
      doc
  | _, _ ->
      Cat (requirement doc1 ++ requirement doc2, doc1, doc2)

let nest (i : int) (doc : doc) : doc =
  assert (i >= 0);
  Nest (i, requirement doc, doc)

let group (doc : doc) : doc =
  Group (requirement doc, doc)
