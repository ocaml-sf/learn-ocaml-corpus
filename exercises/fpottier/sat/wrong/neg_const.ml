let var x =
  FVar x

let falsity =
  FConst false

let truth =
  FConst true

let const sense =
  if sense then truth else falsity
    (* [FConst sense] would work too, but would allocate memory *)

let neg f =
  match f with
  (* wrong: failure to recognize FConst *)
  | FNeg f ->
      f
  | _ ->
      FNeg f

let conn sense f1 f2 =
  match f1, f2 with
  | FConst sense', f
  | f, FConst sense' ->
      if sense = sense' then
        f
      else
        FConst sense'
  | _, _ ->
      FConn (sense, f1, f2)

let conj f1 f2 =
  conn true f1 f2

let disj f1 f2 =
  conn false f1 f2
