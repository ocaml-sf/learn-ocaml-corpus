let is_empty = function
  | ([], []) -> true
  | _ -> false

let enqueue x (front, back) = (front, x :: back)

let split l =
  let rec aux k accu l =
    if k = 0 then
      (List.rev accu, List.rev l)
    else match l with
      | [] -> (List.rev accu, List.rev l)
      | x :: xs -> aux (k - 1) (x :: accu) xs
  in
  aux (List.length l / 2) [] (List.rev l)

let rec dequeue (front, back) =
  match front with
    | x :: front' -> (x, (front', back))
    | [] -> match back with
      | [] -> assert false
      | [x] -> (x, ([], []))
      | back -> dequeue (split back)
