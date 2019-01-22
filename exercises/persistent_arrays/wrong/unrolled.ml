let make (n : int) (x : 'a) : 'a parray =
  let data = Array.make n x in
  ref (Array { data })

let set (base : 'a parray) (i : int) (value : 'a) =
  ref (Apply { base; i; value })

let rec revert (a : 'a parray) : 'a array =
  match !a with
  | Array { data } ->
      data
  | Apply { base; i; value } ->
      let data = revert base in
      let v = data.(i) in
      data.(i) <- value;
      a := Array { data };
      (* base := Apply { base = a; i; value = v }; *) (* wrong *)
      data

let get (a : 'a parray) (j : int) : 'a =
  let data =
    (* One unrolling of [revert]. *)
    match !a with
    | Array { data } ->
        data
    | Apply { base; i; value } ->
        let data = revert base in
        let v = data.(i) in
        data.(i) <- value;
        a := Array { data };
        base := Apply { base = a; i; value = v };
        data
  in
  data.(j)
