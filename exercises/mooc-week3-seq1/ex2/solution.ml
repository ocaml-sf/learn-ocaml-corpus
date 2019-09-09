let rec mem x = function
  | [] -> false
  | y :: ys -> x = y || mem x ys

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | x :: xs -> x :: append xs l2

let rec combine l1 l2 =
  match (l1, l2) with
    | ([], []) -> []
    | (x :: xs, y :: ys) -> (x, y) :: combine xs ys
    | _ -> assert false

let rec assoc l k =
  match l with
    | [] -> None
    | (k', x) :: l -> if k = k' then Some x else assoc l k
