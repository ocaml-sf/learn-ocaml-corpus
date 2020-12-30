let make () : elem =
  ref (Root 0)

let rec find (x : elem) : elem =
  match !x with
  | Root _ ->
      x
  | Link y ->
      find y

let eq (x : elem) (y : elem) : bool =
  find x == find y

let link (x : elem) (y : elem) : unit =
  x := Link y (* wrong; will lead to non-termination *)

let union (x : elem) (y : elem) : unit =
  link (find x) (find y)
