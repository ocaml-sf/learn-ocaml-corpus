let halve l =
  let len = List.length l in
  let rec halve_aux (acc, acc2) i = function
    | [] -> (acc |> List.rev, acc2 |> List.rev)
    | x :: xs -> if i < (len / 2) then halve_aux ((x :: acc), acc2) (i + 1) xs
        else halve_aux (acc, (x :: acc2)) (i + 1) xs
  in
  halve_aux ([], []) 0 l
