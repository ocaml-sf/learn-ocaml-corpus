(* ------------------------------------------------------------------------------ *)

(* Building formulae. *)

let var x =
  (* TO DO: Define this function. *)
  raise TODO

let falsity =
  FConst false

let truth =
  FConst true

let const sense =
  (* TO DO: Define this function. *)
  raise TODO

let neg f =
  (* TO DO: Define this function. *)
  raise TODO

let conn sense f1 f2 =
  (* TO DO: Define this function. *)
  raise TODO

let conj f1 f2 =
  conn true f1 f2

let disj f1 f2 =
  conn false f1 f2

(* ------------------------------------------------------------------------------ *)

(* Evaluating formulae. *)

let rec eval (env : env) (f : formula) : bool =
  (* TO DO: Define this function. *)
  raise TODO

let foreach_env (n : int) (body : env -> unit) : unit =
  (* TO DO: Define this function. *)
  raise TODO

let satisfiable (n : int) (f : formula) : bool =
  (* TO DO: Define this function. *)
  raise TODO

let valid (n : int) (f : formula) : bool =
  (* TO DO: Define this function. *)
  raise TODO

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
    (* TO DO: Define this function. *)
    raise TODO

  (* The clause returned by [clause s f c], in conjunction with any
     emitted clauses, must be logically equivalent to [s.f \/ c]. *)

  and clause (s : bool) (f : formula) (c : clause) : clause =
    (* TO DO: Define this function. *)
    raise TODO

  (* The main entry point. *)

  let cnf (f : formula) : unit =
    decompose true f empty

end

(* -------------------------------------------------------------------------- *)

(* Recreation: determining whether two sorted lists have a common element. *)

let rec intersect (xs : int list) (ys : int list) : bool =
  (* TO DO: Define this function. *)
  raise TODO

(* ------------------------------------------------------------------------------ *)

(* Representing a set of clauses in memory. *)

exception UNSAT

module Clauses () = struct

  (* A clause is represented as a list of literals. *)

  type literal = bool * var
  type clause = literal list

  let empty : clause =
    (* TO DO: Define this constant. *)
    raise TODO

  let cons p x clause : clause =
    (* TO DO: Define this function. *)
    raise TODO

  (* Clauses are numbered and are stored in an infinite array. *)

  let clauses : clause option InfiniteArray.t =
    InfiniteArray.make None

  let new_clause (clause : clause) : unit =
    (* TO DO: Define this function. *)
    raise TODO

  let count_clauses () : int =
    (* TO DO: Define this function. *)
    raise TODO

  let new_var () : var =
    (* TO DO: Define this function. *)
    raise TODO

end

(* ------------------------------------------------------------------------------ *)

(* The undo trail. *)

module Trail () : sig
  val push: (unit -> unit) -> unit
  type checkpoint
  val record: unit -> checkpoint
  val revert: checkpoint -> unit
end = struct

  let push action =
    (* TO DO: Define this function. *)
    raise TODO

  type checkpoint
    (* TO DO: Define this type. *)

  let record () =
    (* TO DO: Define this function. *)
    raise TODO

  let revert i =
    (* TO DO: Define this function. *)
    raise TODO

end

(* ------------------------------------------------------------------------------ *)

(* A set of variables. *)

module VarSet () : sig
  val mem: var -> bool
  val add: var -> unit
  val remove: var -> unit
  val pick: unit -> var option
end = struct

  let mem x =
    (* TO DO: Define this function. *)
    raise TODO

  let add x =
    (* TO DO: Define this function. *)
    raise TODO

  let remove x =
    (* TO DO: Define this function. *)
    raise TODO

  let pick () =
    (* TO DO: Define this function. *)
    raise TODO

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
    (* TO DO: Define this function. *)
    raise TODO

  let set_clause (i : int) (clause : C.clause option) : unit =
    (* TO DO: Define this function. *)
    raise TODO

  let set_value (x : var) (p : bool) : unit =
    (* TO DO: Define this function. *)
    raise TODO

  let find_unit_clause () : C.literal option =
    (* TO DO: Define this function. *)
    raise TODO

  let rec propagate () : unit =
    (* TO DO: Define this function. *)
    raise TODO

  let rec explore () : unit =
    (* TO DO: Define this function. *)
    raise TODO

  let solve (n : int) (f : formula) : (var -> bool) option =
    (* Declare the existence of [n] variables. *)
    for _i = 0 to n-1 do
      ignore (C.new_var())
    done;
    (* TO DO: Complete the definition of this function. *)
    raise TODO

end
