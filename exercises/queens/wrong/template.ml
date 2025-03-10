let rec select (xs : 'a list) : ('a * 'a list) m =
  (* TO DO: Define this function. *)
  raise TODO

let rec unfold : type a s . (s -> bool) -> (s -> (a * s) m) -> s -> a list m =
  fun final step s ->
    (* TO DO: Define this function. *)
    raise TODO

let permutations (xs : 'a list) : 'a list m =
  (* TO DO: Define this function. *)
  raise TODO (* one line *)

let nodup xs =
  (* TO DO: Define this function. *)
  raise TODO

let safe (placement : placement) : bool =
  (* TO DO: Define this function. *)
  raise TODO

let queens (n : int) : placement m =
  (* TO DO: Define this function. *)
  raise TODO

let queenz (n : int) : placement m =
  (* TO DO: Define this function. *)
  raise TODO
