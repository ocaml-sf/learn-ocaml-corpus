let make (n : int) (x : 'a) : 'a parray =
  let data = Array.make n x in
  ref (Array { data })

let set (base : 'a parray) (i : int) (value : 'a) =
  base (* wrong *)

let rec revert (a : 'a parray) : 'a array =
  match !a with
  | Array { data } ->
      data
  | Apply { base; i; value } ->
      assert false

let get (a : 'a parray) (i : int) : 'a =
  let data = revert a in
  data.(i)
