let length (type a) (s : a seq) : int =
  match s with
  | Empty ->
      0
  | Singleton _ ->
      1
  | Sum (length, _, _) ->
      length
  | Product (length, _, _) ->
      length
  | Map (length, _, _) ->
      length

let is_empty s =
  length s = 0

let empty =
  Empty

let singleton x =
  Singleton x

let sum s1 s2 =
  if is_empty s1 then s2
  else if is_empty s2 then s1
  else Sum (length s1 + length s2, s1, s2)

let product s1 s2 =
  if is_empty s1 || is_empty s2 then
    empty
  else
    Product (length s1 * length s2, s1, s2)

let map phi s =
  if is_empty s then
    empty
  else
    Map (length s, phi, s)

let rec get : type a . a seq -> int -> a =
  fun s i ->
    raise TODO

let rec foreach : type a . a seq -> (a -> unit) -> unit =
  fun s k ->
    raise TODO
