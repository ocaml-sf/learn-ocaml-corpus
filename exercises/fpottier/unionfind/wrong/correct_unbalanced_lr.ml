(* This code uses neither path compression nor balancing.
   [union x y] always installs a link of [x] to [y]. *)

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
    x := Link y (* wrong *)

let union (x : elem) (y : elem) : unit =
  link (find x) (find y)
