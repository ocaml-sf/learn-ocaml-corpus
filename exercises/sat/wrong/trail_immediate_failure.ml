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
  val new_var: unit -> var
  val new_clause: clause -> unit
end)
= struct
  open X

  (* The conjunction of the clauses emitted by [decompose s f c] must be logically
     equivalent to the formula [s.f \/ c]. *)

  (* It is permitted to assume that [c] is small and therefore to duplicate
     it. It is not permitted to duplicate [f]. *)

  let rec decompose (s : bool) (f : formula) (c : clause) : unit =
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
        decompose s f1 c;
        decompose s f2 c
    | FNeg f1 ->
        (* The formula [s.f] is equivalent to [(not s).f1]. *)
        decompose (not s) f1 c
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
        (* Thus, we return [c]. Note, however, that [FConst _] can appear
           only at the toplevel of a formula, so the fact that we are here
           implies that [c] must be [empty]. *)
        assert (c == empty);
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
           [decompose s f (cons false x empty)] works. *)

        (* Because [s.f] is equivalent to the conjunction of [s.f1] and
           [s.f2], this call can be written as a sequence of two recursive
           calls, as follows. This makes it apparent that the recursive
           calls concern subformulae. *)

        decompose s f1 (cons false x empty);
        decompose s f2 (cons false x empty);

        (* Use [x] to stand for [s.f] in this clause. *)
        cons true x c

  (* The main entry point. *)

  let cnf (f : formula) : unit =
    decompose true f empty

end

(* -------------------------------------------------------------------------- *)

(* Recreation: determining whether two sorted lists have a common element. *)

let rec intersect (xs : int list) (ys : int list) : bool =
  match xs, ys with
  | [], _
  | _, [] ->
      false
  | x :: xs, y :: ys ->
      x = y ||
      x < y && intersect xs (y :: ys) ||
      y < x && intersect (x :: xs) ys

(* -------------------------------------------------------------------------- *)

(* Counting. *)

let postincrement (c : int ref) : int =
  let x = !c in
  c := x + 1;
  x

(* ------------------------------------------------------------------------------ *)

(* Representing a set of clauses in memory. *)

exception UNSAT

module Clauses () = struct

  (* A clause is represented as a list of literals. *)

  type literal = bool * var
  type clause = literal list

  let empty : clause =
    []

  let cons p x clause : clause =
    (p, x) :: clause

  (* Clauses are numbered and are stored in an infinite array. *)

  let clauses : clause option InfiniteArray.t =
    InfiniteArray.make None

  (* The counter [c] is used to allocate new clauses. *)

  let c : int ref =
    ref 0

  let new_clause (clause : clause) : unit =
    (* If [clause] contains x \/ ~x, drop this clause altogether. *)
    let neg =
      clause
      |> List.filter (fun (p, _x) -> not p)
      |> List.map snd
      |> List.sort compare
    and pos =
      clause
      |> List.filter (fun (p, _x) -> p)
      |> List.map snd
      |> List.sort compare
    in
    if intersect neg pos then
      ()
    (* If this clause has length 0, fail immediately. *)
    else if clause = [] then
      raise UNSAT
    else begin
      (* Allocate a new clause index. *)
      let i = postincrement c in
      (* Record this clause. *)
      InfiniteArray.set clauses i (Some clause);
    end

  let count_clauses () : int =
    !c

  (* The counter [v] is used to allocate new variables. *)

  let v : var ref =
    ref 0

  let new_var () : var =
    postincrement v

  let count_vars () : int =
    !v

end

(* ------------------------------------------------------------------------------ *)

module Trail () : sig
  val push: (unit -> unit) -> unit
  type checkpoint
  val record: unit -> checkpoint
  val revert: checkpoint -> unit
end = struct

  (* A trivial undo action. *)

  let nothing () = ()

  (* A stack of undo actions. *)

  let actions = InfiniteArray.make nothing

  (* The current stack level. *)

  let current = ref (0/0) (* wrong *)

  (* Installing a new undo action. *)

  let push action =
    InfiniteArray.set actions (postincrement current) action

  (* Recording the current level. *)

  type checkpoint = int

  let record () = !current

  (* Going back to a previously recorded stack level. *)

  let revert i =
    for j = !current - 1 downto i do
      let action = InfiniteArray.get actions j in
      action();
      (* We overwrite the now-empty stack slot with [nothing], in order
         to avoid a memory leak. This is optional. It might be more
         efficient to not do it. *)
      InfiniteArray.set actions j nothing
    done;
    current := i

end

(* ------------------------------------------------------------------------------ *)

(* TEMPORARY *)

module SAT (X : sig val f: formula end) () : sig end = struct
  open X

  module C = Clauses()
  open C

  module F = CNF(C)
  open F
  let () = F.cnf f

  let unresolved : bool InfiniteArray.t =
    InfiniteArray.make true

  let rec pick x =
    if x < count_vars() then
      if InfiniteArray.get unresolved x then Some x else pick (x+1)
    else
      None

  let pick () =
    pick 0

  (* The array [value] maps every resolved variable [x] to its Boolean value. *)

  let value : bool InfiniteArray.t =
    InfiniteArray.make false

  module Trail = Trail()

  let mark_resolved x =
    InfiniteArray.set unresolved x false;
    Trail.push (fun () ->
      InfiniteArray.set unresolved x true
    )

  let update_clause i clause =
    let current = InfiniteArray.get clauses i in
    InfiniteArray.set clauses i clause;
    Trail.push (fun () ->
      InfiniteArray.set clauses i current
    )

end
