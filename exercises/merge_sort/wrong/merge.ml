let rec odd (xs : 'a list) : 'a list =
  match xs with
  | [] ->
      []
  | _ :: xs ->
      even xs

and even (xs : 'a list) : 'a list =
  match xs with
  | [] ->
      []
  | x :: xs ->
      x :: odd xs

let rec merge (xs : int list) (ys : int list) : int list =
  match xs, ys with
  | [], _ ->
      ys
  | _, [] ->
      xs
  | x :: xs, y :: ys ->
      if x <= y then
        x :: merge xs ys (* wrong *)
      else
        y :: merge (x :: xs) ys
