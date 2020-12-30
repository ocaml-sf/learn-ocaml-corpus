(* The data values carried by the leaves of a tree. *)

let rec elements t xs =
  match t with
  | Leaf x ->
      x :: xs
  | Fork (t, u) ->
      (* wrong: reversed *)
      elements u (elements t xs)

let elements (t : 'a tree) : 'a list =
  elements t []
