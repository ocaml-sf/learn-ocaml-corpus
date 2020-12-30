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
    maximum := max !maximum x;
    InfiniteArray.set member x true

  let remove x =
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

  (* Create an (initially empty) set of undecided variables. *)

  module Undecided = VarSet()

  (* Set up an array of all clauses. Override [new_var] so that
     newly created variables are automatically added to the set
     of undecided variables. *)

  module C = struct

    include Clauses()

    let new_var () : var =
      let x = new_var() in
      Undecided.add x;
      x

  end

  (* Create an (initially empty) undo trail. *)

  module Trail = Trail()

  (* Set up an array [value] that maps every decided variable [x]
     to its Boolean value. *)

  let value : bool InfiniteArray.t =
    InfiniteArray.make false

  (* This is where you begin to work... *)

  let mark_decided (x : var) : unit =
    Undecided.remove x;
    Trail.push (fun () -> Undecided.add x)

  let set_clause (i : int) (clause : C.clause option) : unit =
    let current = InfiniteArray.get C.clauses i in
    InfiniteArray.set C.clauses i clause;
    Trail.push (fun () ->
      InfiniteArray.set C.clauses i current
    )

  let decided x =
    not (Undecided.mem x)

  let set_value (x : var) (p : bool) : unit =
    (* This write to the [value] array does not need to be undone. *)
    InfiniteArray.set value x p;
    (* The clauses where [x] appears must now be visited. Those where
       [x] appears with polarity [p] become satisfied. (In particular,
       this can cause a unit clause to become satisfied.) Those where
       [x] occurs with opposite polarity can be simplified. This may
       cause them to become unit clauses. This may also cause them to
       become empty clauses, in which case we have detected a
       contradiction. Note that these two cases are exclusive; we
       cannot have both [x] and [~x] in a clause, as we have excluded
       this situation up front. *)
    for i = 0 to C.count_clauses() - 1 do
      match InfiniteArray.get C.clauses i with
      | None ->
          ()
      | Some clause ->
          if List.mem (p, x) clause then
            (* [x] occurs with polarity [p] in the clause. *)
            set_clause i None
          else if List.mem (not p, x) clause then
            (* [x] occurs with polarity [~p] in the clause. *)
            let clause = List.filter (fun (_, y) -> x <> y) clause in
            if clause = [] then
              (* This clause becomes empty! Fail. *)
              raise UNSAT
            else
              set_clause i (Some clause)
    done

  let find_unit_clause () : C.literal option =
    let exception Found of C.literal in
    try
      for i = 0 to C.count_clauses() - 1 do
        match InfiniteArray.get C.clauses i with
        | Some [lit] -> raise (Found lit) | _ -> ()
      done;
      None
    with Found lit ->
      Some lit

  let rec propagate () : unit =
    (* Find a literal that appears in a unit clause, if there is one. *)
    match find_unit_clause () with
    | None ->
        (* If there is none, then unit propagation is over. *)
        ()
    | Some (p, x) ->
        (* The variable [x] is not yet decided (otherwise this clause would
           have been satisfied or falsified). We now can and must decide it:
           its value should be [p]. This allows us to simplify or satisfy the
           clauses where [x] occurs; in particular, this unit clause is
           satisfied and disappears. *)
        (* mark_decided x; (* wrong *) *)
        set_value x p;
        (* Continue. *)
        propagate()

  let rec explore () : unit =
    (* At this point, there are no unit clauses. If all variables are
       decided, then we are done. Otherwise, we pick an undecided
       variable and attempt to set it to [true]. *)
    match Undecided.pick() with
    | None ->
        (* We are done! The problem is satisfiable. *)
        ()
    | Some x ->
        (* Mark [x] decided. *)
        mark_decided x;
        (* Record our current state, so as to be able to backtrack down to
           this point if necessary. *)
        let here = Trail.record() in
        (* Be prepared to catch [UNSAT], which can be raised by [decide]
           and by [solve]. *)
        try
          (* Set [x] to [true]. *)
          set_value x true;
          propagate();
          explore()
        with UNSAT ->
          (* Failure. Revert to our prior state by executing a segment of the
             undo trail and by emptying the bag of unit clauses. *)
          Trail.revert here;
          (* We now know that (in the context of our previous decisions) our
             latest decision was a mistake. We can therefore make the opposite
             decision and explore its consequences. This may in turn cause us
             to fail and backtrack further down, etc. *)
          set_value x false;
          propagate();
          explore()

  let solve (n : int) (f : formula) : (var -> bool) option =
    (* Declare the existence of [n] variables. *)
    for _i = 0 to n-1 do
      ignore (C.new_var())
    done;
    try
      (* Convert the formula [f] to CNF form. *)
      let module F = CNF(C) in
      F.cnf f;
      (* Run the solver. *)
      propagate();
      explore();
      (* Every variable must now be decided. *)
      let env (x : var) =
        InfiniteArray.get value x
      in
      Some env
    with UNSAT ->
      None

end
