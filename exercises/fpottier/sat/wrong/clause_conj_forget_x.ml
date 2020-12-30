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
  for i = 0 to 1 lsl n - 1 do
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

(* ------------------------------------------------------------------------------ *)

(* Converting formulae to conjunctive normal form. *)

module CNF (X : sig
  type clause
  val empty: clause
  val cons: bool -> var -> clause -> clause
  val new_var: (unit) -> var
  val new_clause: clause -> unit
end)
= struct
  open X

  (* The conjunction of the clauses emitted by [cnf s f c] must be logically
     equivalent to the formula [s.f \/ c]. *)

  (* It is permitted to assume that [c] is small and therefore to duplicate
     it. It is not permitted to duplicate [f]. *)

  let rec cnf (s : bool) (f : formula) (c : clause) : unit =
    match f with
    | FConst s' when s = s' ->
        (* The formula [s.f] is True, therefore the disjunction [s.f \/ c] is
           true. No clauses need be emitted. *)
        ()
    | FConn (s', f1, f2) when s = s' ->
        (* The formula [s.f] can be written as a conjunction [s.f1 /\ s.f2].
           The disjunction [_ \/ c] can be distributed onto this conjunction.
           Thus, the formula [s.f \/ c] is logically equivalent to the
           conjunction of [s.f1 \/ c] and [s.f2 \/ c]. *)
        cnf s f1 c;
        cnf s f2 c
    | FNeg f1 ->
        (* The formula [s.f] is equivalent to [(not s).f1]. *)
        cnf (not s) f1 c
    | _ ->
        (* The formula [s.f] is False or a disjunction or a variable.
           Transform it into a single clause and emit this clause. *)
        new_clause (clause s f c)

  (* The clause returned by [clause s f c], in conjunction with any
     emitted clauses, must be logically equivalent to [s.f \/ c]. *)

  and clause (s : bool) (f : formula) (c : clause) : clause =
    match f with
    | FConst s' when s <> s' ->
        (* The formula [s.f] is False, so [s.f \/ c] is equivalent to [c]. *)
        c
    | FConn (s', f1, f2) when s <> s' ->
        (* The formula [s.f] can be written as the disjunction [s.f1 \/ s.f2].
           Therefore [s.f \/ c] is equivalent to [s.f1 \/ s.f2 \/ c]. *)
        clause s f1 (clause s f2 c)
    | FNeg f1 ->
        (* The formula [s.f] is equivalent to [(not s).f1]. *)
        clause (not s) f1 c
    | FVar x ->
        (* [s.x \/ c] is a valid clause. Return it. *)
        cons s x c
    | FConst s' ->
        assert (s = s');
        (* The formula [s.f] is True. Our assumption is violated. *)
        assert false
    | FConn (s', f1, f2) ->
        assert (s = s');

        (* The formula [s.f] is a conjunction. It cannot appear in a clause.
           We must create a proxy, that is, a fresh variable [x] that stands
           for [s.f], and use [x] in the current clause. We emit auxiliary
           clauses to indicate that [x] implies [s.f]. As noted by Plaisted
           and Greenbaum, the reverse implication is not necessary. *)

        let x = new_var () in

            (* Emit clauses to indicate that [x] implies [s.f]. Because this
               implication is equivalent to [s.f \/ ~x], a single call to
               [cnf s f (cons false x empty)] works. *)

            (* Because [s.f] is equivalent to the conjunction of [s.f1] and
               [s.f2], this call can be written as a sequence of two recursive
               calls, as follows. This makes it apparent that the recursive
               calls concern subformulae. *)

            let c' = cons false x empty in
            cnf s f1 c';
            cnf s f2 c';

        (* Use [x] to stand for [f] in this clause. *)
        c (* wrong *)

  (* The main entry point. *)

  let cnf (f : formula) : unit =
    cnf true f empty

end
