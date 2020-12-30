(* -------------------------------------------------------------------------- *)

(* Comparing and printing sequences. *)

module SeqExtra = struct

  let equal_upto depth xs ys =
    Seq.equal (=) (Seq.take depth xs) (Seq.take depth ys)

  let print_upto print_element depth xs =
    let rec contents depth xs =
      match depth, xs() with
      | _, Seq.Nil ->
          []
      | 0, Seq.Cons _ ->
          [ utf8string "..." ]
      | _, Seq.Cons (x, xs) ->
          print_element x :: contents (depth - 1) xs
    in
    group (semis (contents depth xs))

end
