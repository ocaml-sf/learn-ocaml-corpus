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
        x :: merge xs (y :: ys)
      else
        y :: merge (x :: xs) ys

      (* Note: we reconstruct the lists [y :: ys] and [x :: xs] on the
         right-hand side because this seems simplest and most elegant.
         A good compiler can perform CSE and find out that there is no
         need to reallocate these list cells because they are already
         at hand. *)

let rec sort (xs : int list) : int list =
  match xs with
  | [] | [_] ->
      xs
  | _ ->
      merge (sort (even xs)) (sort (odd xs))

let rec uniq (xs : int list) : int list =
  match xs with
  | [] | [_] ->
      xs
  | x0 :: x1 :: xs ->
      if x0 = x1 then
        uniq (x1 :: xs)
      else
        x0 :: uniq (xs) (* wrong, dropping x1 *)
