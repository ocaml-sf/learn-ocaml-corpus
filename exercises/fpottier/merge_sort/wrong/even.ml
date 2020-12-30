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
      x :: even xs (* wrong *)
