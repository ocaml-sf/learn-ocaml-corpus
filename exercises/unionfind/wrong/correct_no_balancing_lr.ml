(* This code uses path compression, but no balancing.
   [union x y] creates a link of [x] to [y]. *)

let make () : elem =
  ref (Root 0)

let rec find (x : elem) : elem =
  match !x with
  | Root _ ->
      x
  | Link y ->
      let z = find y in
      x := Link z;
      z

let eq (x : elem) (y : elem) : bool =
  find x == find y

let link (x : elem) (y : elem) : unit =
  if x != y then
    match !x, !y with
    | Root rx, Root ry ->
        x := Link y
    | Root _, Link _
    | Link _, Root _
    | Link _, Link _ ->
        assert false

let union (x : elem) (y : elem) : unit =
  link (find x) (find y)
