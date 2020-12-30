(* ------------------------------------------------------------------------------ *)

(* Building formulae. *)

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
  | FConst sense ->
      const (not sense)
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

(* ------------------------------------------------------------------------------ *)

(* Evaluating formulae. *)

let rec eval (env : env) (f : formula) : bool =
  match f with
  | FConst b ->
      b
  | FConn (true, f1, f2) ->
      eval env f1 && eval env f2
  | FConn (false, f1, f2) ->
      eval env f1 || eval env f2
  | FNeg f ->
      not (eval env f)
  | FVar x ->
      env x

let foreach_env (n : int) (body : env -> unit) : unit =
  (* We assume [n < Sys.word_size - 1]. *)
  for i = 0 to 1 lsl n - 2 do (* wrong *)
    let env x =
      assert (0 <= x && x < n);
      i land (1 lsl x) <> 0
    in
    body env
  done

let satisfiable (n : int) (f : formula) : bool =
  let exception SAT in
  try
    foreach_env n (fun env ->
      if eval env f then
        raise SAT
    );
    false
  with SAT ->
    true

let valid (n : int) (f : formula) : bool =
  not (satisfiable n (neg f))
