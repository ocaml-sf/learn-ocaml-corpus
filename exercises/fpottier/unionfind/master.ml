let make () : elem =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  ref (Root 0)
     END EXCLUDE *)

let rec find (x : elem) : elem =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  match !x with
  | Root _ ->
      x
  | Link y ->
      let z = find y in
      if z != y then (* optional test: saves one allocation and one write when successful *)
        x := Link z;
      z
     END EXCLUDE *)

let eq (x : elem) (y : elem) : bool =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  find x == find y
    (* Note that we could further optimize [eq] by testing the physical
       equality [x == y] before evaluating [find x] and [find y]. *)
     END EXCLUDE *)

(* BEGIN EXCLUDE
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
      y := Link x; x := Root (rx+1)
    end

     END EXCLUDE *)
let link (x : elem) (y : elem) : unit =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  (* Note: this function is unused in our solution. *)
  match !x, !y with
  | Root rx, Root ry ->
      link_rank (x, rx) (y, rx)
  | Link _, _
  | _, Link _ ->
      assert false
     END EXCLUDE *)

let union (x : elem) (y : elem) : unit =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  link_rank (find_rank x) (find_rank y)
     END EXCLUDE *)
