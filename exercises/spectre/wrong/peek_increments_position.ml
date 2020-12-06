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
  depths 0 t []

(* The elements and depths, combined. *)

let spectre (t : 'a tree) : 'a spectre =
  List.combine (elements t) (depths t)
    (* A direct definition can also be given. *)

(* A facility for reading and consuming the elements of a list. *)

let new_input (xs : 'a list) : 'a input =
  let input, position = ref xs, ref 0 in
  let peek () =
    match !input with [] -> None | x :: _ -> incr position; (* wrong *) Some x
  and consume () =
    match !input with [] -> assert false | _ :: xs -> input := xs; incr position
  and current () =
    !position
  in
  { peek; consume; current }
