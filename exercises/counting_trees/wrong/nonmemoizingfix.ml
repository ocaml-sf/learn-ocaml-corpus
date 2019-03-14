let fix : type a b . ((a -> b) -> (a -> b)) -> (a -> b) =
  fun ff ->
    let table = Hashtbl.create 128 in
    let rec f x =
      try
        Hashtbl.find table x
      with Not_found ->
        let y = ff f x in
        (* Hashtbl.add table x y; *) (* forget to record *)
        y
    in
    f
