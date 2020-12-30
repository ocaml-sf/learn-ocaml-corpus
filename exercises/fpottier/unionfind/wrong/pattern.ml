(* This example attempts to use pattern-matching on a reference
   in order to circumvent the way in which we count read operations.
   It is ill-typed because we have redefined the type [ref] as an
   abstract type. *)

let make () : elem =
  ref (Root 0)

(* wrong: this should be ill-typed *)
let (!) ({ contents } : elem) =
  contents

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
        if rx < ry then begin
          x := Link y
        end else if rx > ry then begin
          y := Link x
        end else begin
          y := Link x; x := Root (rx+1)
        end
    | Root _, Link _
    | Link _, Root _
    | Link _, Link _ ->
        assert false

let union (x : elem) (y : elem) : unit =
  link (find x) (find y)
