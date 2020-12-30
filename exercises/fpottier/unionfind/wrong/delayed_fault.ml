(* This file will pass the correctness test
   but fail during the complexity test,
   which uses larger scenarios. *)

let make () : elem =
  ref (Root 0)

let rec find (x : elem) : elem =
  match !x with
  | Root _ ->
      x
  | Link y ->
      let z = find y in
      if z != y then (* optional test: saves one allocation and one write when successful *)
        x := Link z;
      z

let eq (x : elem) (y : elem) : bool =
  find x == find y
    (* Note that we could further optimize [eq] by testing the physical
       equality [x == y] before evaluating [find x] and [find y]. *)

let rec find_rank (x : elem) : elem * rank =
  match !x with
  | Root rx ->
      x, rx
  | Link y ->
      let (z, _) as result = find_rank y in
      x := Link z;
      result

let link_rank ((x, rx) : elem * rank) ((y, ry) : elem * rank) : unit =
  if x != y then
    if rx < ry then begin
      x := Link y
    end else if rx > ry then begin
      y := Link x
    end else begin
      y := Link x; x := Root (rx+1);
      if rx>5 then failwith "oops" (* wrong *)
    end

let link (x : elem) (y : elem) : unit =
  (* Note: this function is unused in our solution. *)
  match !x, !y with
  | Root rx, Root ry ->
      link_rank (x, rx) (y, rx)
  | Link _, _
  | _, Link _ ->
      assert false

let union (x : elem) (y : elem) : unit =
  link_rank (find_rank x) (find_rank y)
