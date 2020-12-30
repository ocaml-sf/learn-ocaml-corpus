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

