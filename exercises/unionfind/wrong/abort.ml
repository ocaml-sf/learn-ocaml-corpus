let make () : elem =
  ref (Root 0)

exception Abort

let rec find (x : elem) : elem =
  match !x with
  | Root rx ->
      if rx = 2 then
        raise Abort;
      x := Root (rx + 1);
      x
  | Link _ ->
      assert false

let eq (x : elem) (y : elem) : bool =
  find x == find y

let link (x : elem) (y : elem) : unit =
  ()

let union (x : elem) (y : elem) : unit =
  link (find x) (find y)
