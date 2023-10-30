(* insère x dans une liste triée l *)
let rec insert x = function
  | Nothing -> OneMore(x, Nothing)
  | OneMore(h, t) ->
    if h < x
    then OneMore(h, insert x t)
    else OneMore(x, OneMore(h, t))

let sort l = fold_right insert l Nothing

let test_sort l =
  snd(fold_right (fun e (mi, est_triee) -> (min e mi, est_triee && e <= mi)) l
        (max_int, true))
