let make () : elem =
  ref (Root 0)

let rec find (x : elem) : elem =
  x

let eq (x : elem) (y : elem) : bool =
  find x == find y

let link (x : elem) (y : elem) : unit =
  if x != y then
    x := Link y

let union (x : elem) (y : elem) : unit =
  link (find x) (find y)
