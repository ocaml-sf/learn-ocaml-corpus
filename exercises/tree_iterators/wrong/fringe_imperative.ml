(* Producing lists. *)

let rec slow_elements (t : 'a tree) : 'a list =
  match t with
  | Leaf ->
      []
  | Node (t0, x, t1) ->
      slow_elements t0 @ x :: slow_elements t1

let rec elements_with (t : 'a tree) (ys : 'a list) : 'a list =
  match t with
  | Leaf ->
      ys
  | Node (t0, x, t1) ->
      elements_with t0 (x :: elements_with t1 ys)

let elements (t : 'a tree) : 'a list =
  elements_with t []

(* Converting a list to an imperative sequence, which works just once. *)

let convert (xs : 'a list) : 'a Seq.t =
  let remainder = ref xs in
  let rec self () =
    match !remainder with
    | [] ->
        Seq.Nil
    | x :: xs ->
        remainder := xs;
        Seq.Cons (x, self)
  in
  self

let fringe (t : 'a tree) : 'a Seq.t =
  convert (elements t)
