(* Comparing sequences. *)

let rec to_list (xs : 'a Seq.t) : 'a list =
  match xs() with
  | Nil ->
      []
  | Cons (x, xs) ->
      x :: to_list xs

let rec equal (xs : 'a Seq.t) (ys : 'a Seq.t) : bool =
  to_list xs = to_list ys (* wrong: too eager *)
