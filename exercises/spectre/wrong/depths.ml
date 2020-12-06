(* The data values carried by the leaves of a tree. *)

let rec elements t xs =
  match t with
  | Leaf x ->
      x :: xs
  | Fork (t, u) ->
      elements t (elements u xs)

let elements (t : 'a tree) : 'a list =
  elements t []

(* The depths of the leaves of a tree. *)

let rec depths d t ds =
  match t with
  | Leaf _ ->
      d :: ds
  | Fork (t, u) ->
      depths (d + 1) t (depths (d + 1) u ds)

let depths (t : 'a tree) : depth list =
  depths 1 t [] (* wrong: off by one *)
