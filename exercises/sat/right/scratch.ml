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

end

(* ------------------------------------------------------------------------------ *)

(* The undo trail. *)

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

  let current = ref 0

  (* Installing a new undo action. *)

  let push action =
    InfiniteArray.set actions (postincrement current) action

  (* Recording the current level. *)

  type checkpoint
    = int

  let record () =
   !current

  (* Going back to a previously recorded level. *)

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

(* A set of variables. *)

module VarSet () : sig
  val mem: var -> bool
  val add: var -> unit
  val remove: var -> unit
  val pick: unit -> var option
end = struct

  (* Several implementations of the above API are possible. For instance, one
     could simply use a reference to an immutable set of integers, as provided
     by OCaml's standard library. It seems tempting, however, to maintain an
     array of Booleans, so as to allow [mem], [add] and [remove] to operate
     in constant time. A naive implementation of [pick] then requires a linear
     scan. This is the approach used below. One could additionally store a
     linked list of elements, so as to allow [pick] to run in amortized constant
     time. (One would then perform lazy deletion: the linked list would not be
     updated by [remove], but would be updated by a subsequent [pick].) *)

  (* In the simple approach used below, [pick] requires a linear scan. Where
     should this scan stop? We could require this information to be passed as
     an argument to the functor [VarSet]. However, this can be avoided via a
     cute trick: [add] records the greatest element that was ever added to the
     set. *)

  let member : bool InfiniteArray.t =
    InfiniteArray.make false

  let maximum =
    ref (-1)

  let mem x =
    InfiniteArray.get member x

  let add x =
    assert (not (mem x));
    maximum := max !maximum x;
    InfiniteArray.set member x true

  let remove x =
    assert (mem x);
    InfiniteArray.set member x false

  let rec pick_from x =
    if x > !maximum then
      None
    else if mem x then
      Some x
    else
      pick_from (x+1)

  let pick () =
    pick_from 0

end

(* ------------------------------------------------------------------------------ *)

(* The SAT solver. *)

(* The functor application [SAT()] initializes a new solver. This solver
   offers a function [solve], which must be called at most once, because
   it modifies the solver's internal state. *)

module SAT () : sig
  (* [solve n f] determines whether the formula [f], whose variables must be
     comprised between 0 included and [n] excluded, is satisfiable. If it is
     satisfiable, then it returns a solution, in the form of a function of
     variables to Boolean values. Otherwise, it returns [None]. *)
  val solve: int -> formula -> (var -> bool) option
end = struct

  (* Here, we decide to depart from solution.ml and instead include the
     (somewhat more efficient) SAT solver developed in scratch/sat/src. *)

  module CNFMake (X : sig
    type clause
    val empty: clause
    val cons: bool -> var -> clause -> clause
    val new_var: (var -> unit) -> var
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

    (* It is permitted to assume that [s.f] is not True. This requirement is
       satisfied by the recursive calls in the cases [FConn] and [FNeg] because
       an immediate subformula of [FConn] and [FNeg] cannot be [FConst _]. *)

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

          let x = new_var (fun x ->

              (* Emit clauses to indicate that [x] implies [s.f]. Because this
                 implication is equivalent to [s.f \/ ~x], a single call to
                 [cnf s f (cons false x empty)] works. *)

              (* Because [s.f] is equivalent to the conjunction of [s.f1] and
                 [s.f2], this call can be written as a sequence of two recursive
                 calls, as follows. This makes it apparent that the recursive
                 calls concern subformulae. *)

              let c = cons false x empty in
              cnf s f1 c;
              cnf s f2 c

            ) in

          (* Use [x] to stand for [f] in this clause. *)
          cons true x c

    (* The main entry point. *)

    let cnf (f : formula) : unit =
      cnf true f empty

  end

  module IntSet () : sig

    (* [mem x] tests whether [x] is in the set. *)
    val mem: int -> bool

    (* [add x] adds [x] to the set. *)
    (* [x] must not already be a member of the set. *)
    val add: int -> unit

    (* [remove x] removes [x] from the set. *)
    (* [x] must be a member of the set. *)
    val remove: int -> unit

    (* [pick] picks an arbitrary element of the set. This element is not
       removed from the set. *)
    val pick: unit -> int option

  end = struct

    (* For efficiency, we use a redundant representation. The extensible
       array [member] indicates (with certainty) which elements are in
       the set. The bag [members] contains all elements of the set, and
       possibly some integers which are not in the set. *)

    let member =
      InfiniteArray.make false

    let members =
      Stack.create()

    let mem x =
      InfiniteArray.get member x

    let add x =
      assert (not (mem x));
      InfiniteArray.set member x true;
      Stack.push x members

    let remove x =
      assert (mem x);
      InfiniteArray.set member x false;
      (* We cannot efficiently remove [x] from the bag [members],
         so we leave it in there. For this reason, [members] is an
         overapproximation of the set. The spurious elements are
         removed when [extract] stumbles upon them. *)
      (* If [x] happens to be at the top of the stack [members],
         then we can in fact remove it. This (optional) check is
         useful when [add] and [remove] are used in a symmetric
         manner. It prevents the bag [members] from growing too
         much in that case. *)
      if Stack.top members = x then
        ignore (Stack.pop members)

    let rec pick () =
      if Stack.is_empty members then
        None
      else
        (* Pick an element [x] in the bag [members]. *)
        let x = Stack.top members in
        (* If [x] is indeed in the set, we are done. Otherwise, we remove [x]
           from the bag and try again. The cost of this loop can be charged to
           [remove], so every operation has constant amortized complexity. *)
        if mem x then Some x else begin
          InfiniteArray.set member x false;
          ignore (Stack.pop members);
          pick()
        end

  end

  module SATMake(X : sig val verbose : bool end) () = struct
  open X

  let log format =
    Printf.kprintf (fun s -> if verbose then begin print_string s; flush stdout; end) format

  let () =
    log "\n"

  let get = InfiniteArray.get
  let set = InfiniteArray.set

  (* -------------------------------------------------------------------------- *)

  (* Utilities. *)

  let postincrement (c : int ref) : int =
    let x = !c in
    c := x + 1;
    x

  (* -------------------------------------------------------------------------- *)

  (* Type definitions. *)

  type polarity =
    bool

  type var =
    int

  type literal =
    polarity * var

  type clause =
    literal list

  let empty : clause =
    []

  let cons p x clause : clause =
    (p, x) :: clause

  let size clause =
    match clause with
    | [] ->
        0
    | [ _ ] ->
        1
    | _ :: _ :: _ ->
        2 (* Two means two or more. *)

  (* -------------------------------------------------------------------------- *)

  (* Printers. *)

  let show_var x =
    Printf.sprintf "x%d" x

  let show_polarity p =
    if p then "+" else "-"

  let show_literal (p, x) =
    show_polarity p ^ show_var x

  let show_clause lits =
    match lits with
    | [] ->
        "false"
    | [ lit ] ->
        show_literal lit
    | lit :: lits ->
        List.fold_left (fun accu lit ->
          accu ^ " \\/ " ^ show_literal lit
        ) (show_literal lit) lits

  let show_optional_clause oclause =
    match oclause with
    | None ->
        "true"
    | Some clause ->
        show_clause clause

  (* -------------------------------------------------------------------------- *)

  (* Declaration of new variables and clauses. *)

  (* Clauses are numbered internally. An index is a clause number. *)

  type index =
    int

  (* The array [clauses] maps a clause index to the current form of this clause.
     A clause becomes simpler over time, as the variables that appear in it are
     resolved. A clause can also disappear entirely; this means that it has
     become satisfied. *)

  (* An empty clause [Some []] means [false], whereas a missing clause [None]
     means [true]. *)

  (* We maintain the invariant that all of the variables that appear in a
     clause are unresolved. *)

  let clauses : clause option InfiniteArray.t =
    InfiniteArray.make None

  (* The counter [c] is used to allocate new clauses. *)

  let c : index ref =
    ref 0

  (* The array [positive] maps a variable to the indices of the clauses
     where this variable occurs positively. We do not update this array as
     we make progress towards a solution, so: 1- it should be looked up at
     unresolved variables only; 2- the list [positive.(x)] can contain
     clauses that have become true, so [x] no longer appears in them. *)

  let positive : index list InfiniteArray.t =
    InfiniteArray.make []

  (* The array [negative] maps a variable to the indices of the clauses where
     this variable occurs negatively. *)

  let negative : index list InfiniteArray.t =
    InfiniteArray.make []

  (* The expression [(occurrences polarity).(x)] denotes the indices of
     the clauses where the literal [(polarity, x)] occurs. It is also a
     left-value, i.e., it can be used on the left-hand side of an assignment. *)

  let occurrences (p : polarity) =
    if p then positive else negative

  (* The Boolean flag [trivially_unsatisfiable] is used to record the existence
     of an empty clause in the problem that is submitted to us by the user. If
     this is the case, then there is nothing to do. *)

  let trivially_unsatisfiable =
    ref false

  (* The bag [unit] holds the indices of the unit clauses (that is, the clauses
     that have exactly one literal). Because a unit clause can become satisfied
     while it is in this bag, we must also be prepared for this bag to contain
     indices of clauses that have disappeared. *)

  let unit : index Stack.t =
    Stack.create()

  (* Determining whether two sorted lists have a common element. *)

  let rec intersect xs ys =
    match xs, ys with
    | [], _
    | _, [] ->
        false
    | x :: xs, y :: ys ->
        x = y ||
        x < y && intersect xs (y :: ys) ||
        y < x && intersect (x :: xs) ys

  (* A new clause is declared as follows. *)

  let new_clause clause =
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
    else begin
      (* Allocate a new clause index. *)
      let i = postincrement c in
      (* Record this clause. *)
      log "Initializing clause %d to %s.\n" i (show_clause clause);
      set clauses i (Some clause);
      (* Record which literals occur in this clause. *)
      List.iter (fun (p, x) ->
        (* Record that the literal [(p, x)] occurs in clause [i]. *)
        let occurrences = occurrences p in
        set occurrences x (i :: get occurrences x)
      ) clause;
      (* If this clause is empty (therefore unsatisfiable), record this fact.
         This means that the problem is trivially unsatisfiable. *)
      if clause = [] then
        trivially_unsatisfiable := true;
      (* If this is a unit clause, record this fact. *)
      if size clause = 1 then
        Stack.push i unit;
    end

  (* The counter [v] is used to allocate new variables. *)

  let v : var ref =
    ref 0

  (* The set [Unresolved] contains all unresolved variables. A variable is
     unresolved when its value is undecided: that is, it has not yet been
     set to [false] or [true]. *)

  module Unresolved =
    IntSet()

  (* The array [value] maps every resolved variable [x] to its Boolean value. *)

  let value : bool InfiniteArray.t =
    InfiniteArray.make false

  (* A new variable is created as follows. *)

  let new_var (action : var -> unit) =
    let x : var = postincrement v in
    log "Declaring a new variable: %s.\n" (show_var x);
    Unresolved.add x;
    action x; (* TEMPORARY delay action *)
    x

  (* -------------------------------------------------------------------------- *)

  (* It is possible to discover at some point that the problem is unsatisfiable.
     Thus, we must be prepared to undo the changes made during unit propagation
     to our global state, namely to the set [Unresolved] and to the array
     [clauses]. For this purpose, we use an explicit undo trail. (This turns
     out to be more convenient than an approach based purely on exceptions and
     higher-order functions, as this approach forces us into CPS style.) *)

  module Trail =
    Trail()

  let mark_resolved x =
    Unresolved.remove x;
    Trail.push (fun () ->
      log "  Un-resolving %s.\n" (show_var x);
      Unresolved.add x
    )

  let update_clause i clause =
    let current = get clauses i in
    log "Simplifying clause %d to %s.\n" i (show_optional_clause clause);
    set clauses i clause;
    Trail.push (fun () ->
      log "  Restoring clause %d to %s.\n" i (show_optional_clause current);
      set clauses i current
    )

  (* -------------------------------------------------------------------------- *)

  (* Unit propagation. *)

  exception UNSAT

  let resolved x =
    not (Unresolved.mem x)

  let resolve x p : unit =
    (* This write to the [value] array does not need to be undone. *)
    set value x p;
    (* The clauses where [x] appears must now be visited. Those where
       [x] appears with polarity [p] become satisfied. (In particular,
       this can cause a unit clause to become satisfied.) *)
    List.iter (fun i ->
      if get clauses i <> None then (* optional test *)
        update_clause i None
    ) (get (occurrences p) x);
    (* Those where [x] occurs with opposite polarity can be simplified.
       This may cause them to become unit clauses. This may also cause
       them to become empty clauses, in which case we have detected a
       contradiction. *)
    List.iter (fun i ->
      match get clauses i with
      | None ->
          ()
      | Some clause ->
          (* The manner in which this is written relies on the fact that
             we cannot have both [x] and [~x] in a clause. It suffices to
             check for the equality [x = y], ignoring the polarity of [y]. *)
          let clause = List.filter (fun (_, y) -> x <> y) clause in
          update_clause i (Some clause);
          match size clause with
          | 0 ->
              (* This clause becomes empty! Fail. *)
              log "Raising UNSAT.\n";
              raise UNSAT
          | 1 ->
              (* This clause becomes a unit clause. *)
              log "Clause %d is now a unit clause.\n" i;
              Stack.push i unit
          | _ ->
              ()
    ) (get (occurrences (not p)) x)

  let rec propagate () =
    (* Pick a unit clause [i]. *)
    if not (Stack.is_empty unit) then
      let i = Stack.pop unit in
      match get clauses i with
      | None ->
          (* This clause has been satisfied already. Forget about it. *)
          propagate()
      | Some [] ->
          (* This clause has been falsified already. Impossible; we would
             have aborted. *)
          assert false
      | Some (_ :: _ :: _) ->
          (* This is not a unit clause. Impossible; we would not have put
             it in the bag. *)
          assert false
      | Some [ (p, x) ] ->
          (* This is a unit clause. *)
          (* The variable [x] is not yet resolved (otherwise this clause would
             have been satisfied or falsified). We now can and must resolve it:
             its value should be [p]. This allows us to simplify or satisfy the
             clauses where [x] occurs; in particular, this unit clause is
             satisfied and disappears. *)
          log "Resolving %s to %b thanks to unit propagation.\n" (show_var x) p;
          mark_resolved x;
          resolve x p;
          (* Continue. *)
          propagate()

  (* The main recursive function. *)

  let rec explore () =
    (* At this point, there must be no unit clauses. If all variables are
       resolved, then we are done. Otherwise, we pick an unresolved variable
       and attempt to set it to [true]. *)
    assert (Stack.is_empty unit);
    match Unresolved.pick() with
    | None ->
        (* We are done! The problem is satisfiable. *)
        ()
    | Some x ->
        log "Picking a variable: %s.\n" (show_var x);
        (* Mark [x] resolved. *)
        log "Marking %s as resolved.\n" (show_var x);
        mark_resolved x;
        (* Record our current state, so as to be able to backtrack down to
           this point if necessary. *)
        log "Creating a choice point for %s.\n" (show_var x);
        let here = Trail.record() in
        (* Be prepared to catch [UNSAT], which can be raised by [resolve]
           and by [solve]. *)
        try
          (* Set [x] to [true]. *)
          log "Setting %s to true.\n" (show_var x);
          resolve x true;
          propagate();
          explore()
        with UNSAT ->
          (* Failure. Revert to our prior state by executing a segment of the
             undo trail and by emptying the bag of unit clauses. *)
          log "Reverting to the choice point for %s.\n" (show_var x);
          Trail.revert here;
          Stack.clear unit;
          (* We now know that (in the context of our previous decisions) our
             latest decision was a mistake. We can therefore make the opposite
             decision and explore its consequences. This may in turn cause us
             to fail and backtrack further down, etc. *)
          assert (resolved x);
          log "Setting %s to false.\n" (show_var x);
          resolve x false;
          propagate();
          explore()

  (* The main function runs the solver and exports its result. *)

  let solve () : (var -> bool) option =
    if !trivially_unsatisfiable then
      None
    else
      try
        propagate();
        explore();
        (* Every variable must now be resolved. *)
        let env (x : var) =
          assert (resolved x);
          get value x
            (* Subtlety: although we may have created new variables during
               lazy CNF conversion, the user is allowed to query only the
               variables that existed initially. (Clarify.) *)
        in
        Some env
      with UNSAT ->
        None

  end

  module V = struct
    let verbose = false
  end
  open V

  (* Combine [CNF] and [SAT]. Add a bit of code to confirm that the result
     produced by SAT is correct. For satisfiable formulae, this is easy;
     for unsatisfiable formulae, we use a costly naive algorithm. *)

  let none (_ : var) = ()

  let solve (n : int) (f : formula) =
    let module S = SATMake(V)() in
    let module C = CNFMake(S) in
    for _i = 0 to n-1 do
      ignore (S.new_var none)
    done;
    C.cnf f;
    S.solve()

end
