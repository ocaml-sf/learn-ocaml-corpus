(* BEGIN INCLUDE *)
(* Length. *)

(* TO DO: Define [length]. *)
let length (type a) (s : a seq) : int =
  raise TODO

(* Constructors. *)

(* TO DO: Define [empty]. *)

(* TO DO: Define [singleton]. *)

(* TO DO: Define [sum]. *)

(* TO DO: Define [product]. *)

(* TO DO: Define [map]. *)

(* The remaining two accessors. *)

(* TO DO: Define [get]. *)
let rec get : type a . a seq -> int -> a =
  fun s i ->
    raise TODO

(* TO DO: Define [foreach]. *)
let rec foreach : type a . a seq -> (a -> unit) -> unit =
  fun s k ->
    raise TODO

(*   END INCLUDE *)
(* BEGIN EXCLUDE
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
    (* This implementation of [product] imposes the following invariant:
       in every node of the form [Product (_, s1, s2)], the sequences [s1]
       and [s2] are nonempty. This is exploited in the implementation of
       [get] (below) where we divide by [length s2] without fear of division
       by zero. *)

let map phi s =
  if is_empty s then
    empty
  else
    Map (length s, phi, s)

let rec get : type a . a seq -> int -> a =
  fun s i ->
    match s with
    | Empty ->
        out_of_bounds()
    | Singleton x ->
        if i = 0 then x else out_of_bounds()
    | Sum (_, s1, s2) ->
        let n1 = length s1 in
        if i < n1 then get s1 i
        else get s2 (i - n1)
    | Product (_, s1, s2) ->
        let q, r = i / length s2, i mod length s2 in
        get s1 q, get s2 r
    | Map (_, phi, s) ->
        phi (get s i)

let rec foreach : type a . a seq -> (a -> unit) -> unit =
  fun s k ->
    match s with
    | Empty ->
        ()
    | Singleton x ->
        k x
    | Sum (_, s1, s2) ->
        foreach s1 k;
        foreach s2 k
    | Product (_, s1, s2) ->
        foreach s1 (fun x1 ->
          foreach s2 (fun x2 ->
            k (x1, x2)
          )
        )
    | Map (_, phi, s) ->
        foreach s (fun x -> k (phi x))
     END EXCLUDE *)
