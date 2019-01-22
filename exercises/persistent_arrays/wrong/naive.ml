(* This is the naive solution obtained after Question 3.
   Functionally correct, but inefficient. *)

let make (n : int) (x : 'a) : 'a parray =
  let data = Array.make n x in
  ref (Array { data })

let set (base : 'a parray) (i : int) (value : 'a) =
  ref (Apply { base; i; value })

let rec get (a : 'a parray) (j : int) : 'a =
  match !a with
  | Array { data } ->
      data.(j)
  | Apply { base; i; value } ->
      if i = j then value else get base j
