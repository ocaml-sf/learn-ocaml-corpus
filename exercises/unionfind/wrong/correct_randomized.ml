(* This code is correct, and relies on randomization for balancing. *)
(* It is still missing path compression, so it should be rejected. *)

let make () : elem =
  ref (Root 0)

let rec find (x : elem) : elem =
  match !x with
  | Root _ ->
      x
  | Link y ->
      find y (* no path compression *)

let eq (x : elem) (y : elem) : bool =
  find x == find y

let link (x : elem) (y : elem) : unit =
  if x != y then
    x := Link y

let union (x : elem) (y : elem) : unit =
  if Random.bool() then
    link (find x) (find y)
  else
    link (find y) (find x)
