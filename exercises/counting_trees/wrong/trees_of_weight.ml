let fix : type a b . ((a -> b) -> (a -> b)) -> (a -> b) =
  fun ff ->
    let table = Hashtbl.create 128 in
    let rec f (x : a) : b =
      try
        Hashtbl.find table x
      with Not_found ->
        let y = ff f x in
        Hashtbl.add table x y;
        y
    in
    f

let sigma i j f =
  let sum = ref 0 in
  for x = i to j do
    sum := !sum + f x
  done;
  !sum

let split_weight w f =
  sigma 0 w (fun w1 ->
    let w2 = w - w1 in
    f w1 w2
  )

let trees_of_weight =
  fix (fun trees_of_weight w ->
    if w = 0 then
      1
    else
      split_weight w (* wrong; causes stack overflow *) (fun w1 w2 ->
        trees_of_weight w1 * trees_of_weight w2
      )
  )
