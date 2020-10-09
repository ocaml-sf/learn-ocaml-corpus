# A Simple SAT Solver

In this exercise, we build a SAT solver, that is, an algorithm
that solves the
[Boolean satisfiability problem](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem),
and attempts to do so in an efficient manner.

## Building Formulae

Variables are numbered 0, 1, and so on.

```
  type var = int
```

The syntax of formulae is as follows:

```
  type formula =
    | FConst of bool
    | FConn  of bool * formula * formula
    | FNeg   of formula
    | FVar   of var
```

*Falsity* is represented by `FConst false`.
*Truth* is represented by `FConst true`.
We refer to falsity and truth as *constants*.
The *disjunction* of two formulae `f1` and `f2`
is represented by `FConn (false, f1, f2)`,
while their *conjunction* is represented by
`FConn (true, f1, f2)`.
The *negation* of a formula `f`
is represented by `FNeg f`.
Finally, a Boolean variable `x`
is also a formula,
represented by `FVar x`.

It is worth noting that `FConst false` is
a left and right unit for `FConn (false, _, _)`:
that is, the formulae `FConn (false, FConst false, f)`
and `FConn (false, f, FConst false)` are
logically equivalent to `f`.
Dually, `FConst true` is
a left and right unit for `FConn (true, _, _)`.

The duality between falsity and disjunction on the one hand
and truth and conjunction on the other hand
can sometimes be exploited to avoid code duplication.

We impose the following invariant on the syntax of formulae:

* In a negation `FNeg f`,
  the subformula `f` is never a constant or a negation.

* In a composite formula `FConn (_, f1, f2)`,
  the subformulae `f1` and `f2` are never constants.

This invariant is weak. One could think of stronger invariants, such as
*negation normal form* (NNF), *disjunctive normal form* (DNF), and
*conjunctive normal form* (CNF). However, transforming a formula so as to
satisfy these stronger invariants can be costly. Our invariant can be imposed
by a set of *smart constructors* (that is, functions that construct formulae)
while retaining the property that every constructor has constant time
complexity. The four smart constructors that we need are as follows:

```
  val const: bool -> formula
  val conn: bool -> formula -> formula -> formula
  val neg: formula -> formula
  val var: var -> formula
```

**Question 1.** Implement the above four smart constructors in such a way that
the formulae built by these functions always satisfy the above invariant.

## Evaluating Formulae

We can *evaluate* a formula (that is, compute its meaning, a Boolean value) if
we are given an *environment*, that is, a function that maps every variable to
a Boolean value.

```
  type env = var -> bool
```

**Question 2.** Implement a function `eval` of type `env -> formula -> bool`
such that `eval env f` evaluates the formula `f` under the environment `env`.

A formula is *satisfiable* if it evaluates to `true` under *some* environment.
A formula is *valid* if it evaluates to `true` under *every* environment.

To decide whether a formula is satisfiable, or valid, a naïve approach is to
enumerate all environments and evaluate the formula under each such
environment. If the formula at hand involves `n` variables, numbered from `0`
to `n-1`, then there are `2^n` environments of interest.

**Question 3.** Implement a function `foreach_env` of type
`int -> (env -> unit) -> unit` such that
`foreach_env n body` applies the function `body` successively
to every environment over `n` variables.
Then, implement the functions `satisfiable` and `valid`,
both of which have type `int -> formula -> bool`,
such that,
under the hypothesis that the formula `f` involves `n` variables,
`satisfiable n f` determines whether `f` is satisfiable
and `valid n f` determines whether `f` is valid.

*Note.* `foreach_env` is not directly tested by the automatic grading code.
Only `satisfiable` and `valid` are tested.

*Hint.* One simple approach to implementing `foreach_env` is to proceed by
induction on the number `n`. When `n` is zero, then the task is easy, as there
is only one environment over zero variables. When `n` is nonzero, if one knows
how to enumerate all environments over `n-1` variables, then it is not
difficult to see how one can enumerate all environments over `n` variables.

*Hint.* Another approach to implementing `foreach_env` is to note that the
environments over `n` variables are in a bijection with the numbers between
`0` (included) and `2^n` (excluded). Thus, to enumerate all environments, a
single loop suffices, provided one is capable of converting an index `i` in
`[0, 2^n)` to an environment.

## Converting Formulae to Conjunctive Normal Form

### Conjunctive Normal Form

A formula is in *negation normal form* (NNF) if it does not contain a negation
applied to a conjunction, disjunction, or negation. In other words, in such a
formula, every negation must be applied to a variable.

A formula is in *conjunctive normal form* if it is in negation normal form and
furthermore it does not contain a disjunction applied to a conjunction.

In other words, a formula in conjunctive normal form is a conjunction of
clauses, where a *clause* is a disjunction of literals, and a *literal* is
either a variable or the negation of a variable. This can be summed up by the
following grammar:

```
  (literal) l ::= x | ~x
  (clause)  c ::= l \/ c | false
  (cnf)     f ::= c /\ f | true
```

Here, we write `x` for the variable `x` and `~x` for the negation of the
variable `x`. In the following, we also use the notation `s.x`, where `s`
is a Boolean flag, to denote `x` when `s` is `true` and `~x` when `s` is
`false`. Thus, `s.x` is a literal.

The propositional formulae that arise in real-world situations are often
naturally in conjunctive normal form. Unfortunately, determining whether
a formula in CNF is satisfiable is a hard problem, for which no
polynomial-time algorithm is known to exist.
([3-SAT is NP-complete.](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem))
Therefore, an algorithm that solves this problem and
that runs fast in practice is valuable:
such an algorithm is known as a *SAT solver*.

### CNF Conversion: Principles

An arbitrary formula `f` can be converted to a CNF formula `f'` such that `f`
and `f'` are *equisatisfiable*, that is, `f` is satisfiable if and only if
`f'` is satisfiable. (We implement this conversion in the following question.)
Thus, a SAT solver can be applied also to a formula that is not in conjunctive
normal form.

*Note.* One can obviously determine in linear time whether a formula in
*disjunctive normal form* (DNF) is satisfiable. (Think about it.) However,
converting an arbitrary formula to an equisatisfiable disjunctive normal form
has exponential complexity, so the existence of such a linear-time algorithm
is not of much interest.

How does the CNF conversion work?

First, note that it is easy to convert a formula `f` to an equivalent formula
`f'` in negation normal form (NNF). The idea is to push every negation down
towards the leaves. This exploits
[De Morgan's laws](https://en.wikipedia.org/wiki/De_Morgan%27s_laws):

* `~(f1 /\ f2)` is equivalent to `~f1 \/ ~f2`.

* `~(f1 \/ f2)` is equivalent to `~f1 /\ ~f2`.

This transformation can be performed in linear time. In the following
question, we do not implement it as a separate transformation; instead,
we perform this transformation on the fly, at the same time as the CNF
conversion.

Second, to perform CNF conversion, there remains to eliminate the situations
where a conjunction appears inside a disjunction, as in the formula `a \/ (b
/\ c)`. The idea is to introduce a fresh *auxiliary variable* to stand for the
problematic subformula: here, we introduce a new variable `x` to stand for the
subformula `b /\ c`. In other words, we exploit the following fact:

* `a \/ (b /\ c)` is equivalent to `exists x.( (a \/ x)  /\  (x <-> b /\ c) )`.

In the main formula, `b /\ c` is replaced with `x`, so
`a \/ (b /\ c)` becomes `a \/ x`, which is in CNF form,
as desired. On the side, we use a double implication
`x <-> b /\ c` to express the fact that `x` must have
the same truth value as the conjunction `b /\ c`.

A couple more remarks must be made for the above idea to really work.

First, the use of a double implication `x <-> f` is problematic.
Because double implication does not exist in the syntax of formulae,
it must be encoded as the conjunction `(x -> f) /\ (f -> x)`,
or `(~x \/ f) /\ (~f \/ x)`, where the subformula `f` is
duplicated. Unless we are careful, this duplication could cause an
exponential blowup in the size of formulae.
Fortunately, there is in reality no need for a double implication:
a single implication `x -> f` suffices. Indeed,

* `a \/ (b /\ c)` is equivalent to `exists x.( (a \/ x)  /\  (x -> b /\ c) )`.

Please take some time to understand why this is true.

Second, the existential quantification `exists x.(...)` is not really
indispensable. Indeed, the above equivalence implies that:

* the original formula `a \/ (b /\ c)` is satisfiable if and only if the
  transformed formula `(a \/ x) /\ (x -> b /\ c)` is satisfiable, and

* a satisfying assignment of the transformed formula
  is also a satisfying assignment of the original formula.

In summary, thanks to the above ideas, an arbitrary formula `f` can be
converted to a CNF formula `f'` such that `f` and `f'` are *equisatisfiable*
and a satisfying assignment of `f'` is also a satisfying assignment of `f`.
Furthermore, the size of `f'` is linear with respect to the size of `f`, and
the conversion can be performed in linear time.

### CNF Conversion: Implementation

In the next question, you must implement CNF conversion as a functor,
named `CNF`, whose signature is as follows:

```
   module CNF (X : sig
     type clause
     val empty: clause
     val cons: bool -> var -> clause -> clause
     val new_var: unit -> var
     val new_clause: clause -> unit
   end) : sig
    val cnf: formula -> unit
  end
```

In short, this means that you have access to several functions that allow you
to create new auxiliary variables and to declare new clauses. Using these
facilities, you must define a single function, `cnf`, which receives a formula
as an argument and transforms it to conjunctive normal form.

In greater detail, the facilities to which you have access
can be described as follows.

* There is an abstract type `clause` of clauses. (Recall that a clause is a
  disjunction of literals.) There is a constant `empty`, which represents the
  empty clause. (The empty clause is logically equivalent to the formula
  `false`.) There is also a function `cons`, which builds a nonempty clause:
  if `c` is a clause, then `cons s x c` represents the clause `s.x \/ c`, that
  is, the disjunction of the literal `s.x` and of the clause `c`.

* There is a function `new_var`
  such that `new_var()` creates and returns a fresh variable `x`.

* There is a function `new_clause`
  such that `new_clause c`
  declares the clause `c`.

The functions `new_var` and `new_clause` have a side effect: `new_var` adds
the newly created variable to a growing set of *all auxiliary variables
created so far*; `new_clause c` adds the clause `c` to a growing conjunction
of *all clauses declared so far*.

Using these facilities, you are expected to implement the function `cnf`,
which receives a formula `f` and must construct an equisatisfiable formula
in conjunctive normal form. This construction is carried out via suitable
calls to `new_var` and `new_clause`. No result is returned.

**Question 4.** Implement the function `cnf`.

*Hint.* We suggest writing two auxiliary functions, whose types are as
follows:

```
  val decompose: bool -> formula -> clause -> unit
  val clause: bool -> formula -> clause -> clause
```

(These functions should be mutually recursive.) These functions expect
the same parameters, namely a Boolean polarity `s`, a formula `f`, and
a clause `c`, which together represent the logical formula `s.f \/ c`.
(We write `s.f` for `f` if `s` is `true` and `~f` if `s` is `false`.)

The function `decompose` is expected to convert `s.f \/ c` to CNF, via
suitable calls to `new_var` and `new_clause`. It returns no result.

The function `clause` is expected to convert `s.f \/ c` to a clause,
which it returns. It is also allowed to call `new_var` and `new_clause`
so as to construct other clauses on the side.

*Hint.* The easy case in `decompose` is when `s.f` is a conjunction, because
it then suffices to handle each conjunct independently. The easy case in
`clause` is when `s.f` is a disjunction, because it then suffices to deal with
each disjunct in turn and combine the results into a single clause.

## Recreation: Intersection of Two Sorted Lists

**Question 5.** Implement a function `intersect`
of type `int list -> int list -> bool`
such that `intersect xs ys` is `true`
if and only if the two *sorted* lists
`xs` and `ys` have a common element.

## Representing Clauses

We would now like to choose a representation of clauses in memory and
implement the functions `empty`, `cons`, `new_clause` and `new_var`
that the functor `CNF` (above) requires.

We adopt the convention that a literal is represented as a pair of a Boolean
polarity and a variable:

```
  type literal = bool * var
```

We adopt the convention that a clause is represented as a list of literals:

```
  type clause = literal list
```

**Question A.** (Ungraded.) Implement the constant `empty` and the constructor
function `cons`.

We adopt the convention that clauses are numbered
and are stored in an infinite array `clauses`.
Infinite arrays are described at the very end of this exercise.

**Question B.** (Ungraded.) Implement the function `new_clause`, which records a
new clause, and the function `count_clauses`, which indicates how many clauses
have been recorded so far. Note that a clause that contains both the literal
`x` and the opposite literal `~x` is unsatisfiable. In such a situation, the
function `new_clause` must raise the exception `UNSAT`. *Hint:* to detect this
situation, use `intersect`.

**Question C.** (Ungraded.) Implement the function `new_var`, which creates and
returns a new variable.

## The Undo Trail

A SAT solver searches a tree of candidate solutions to a Boolean constraint.
Repeatedly, it picks a variable `x` whose value has not yet been decided, sets
this variable to `true` (for instance), and explores the consequences of this
decision, which form a subtree. If it appears after a while that this decision
was wrong (that is, there is no solution in this subtree) then the algorithm
must *backtrack*, that is, come back to the point where this decision was
made, undo this decision, make the opposite decision (that is, set `x` to
`false`), and explore the consequences of this new decision, which form
another subtree.

A solver usually maintains several mutable data structures, such as a set of
undecided variables, a set of unit clauses, etc. When the solver backtracks to
an earlier point in time, it must reestablish an earlier state of these data
structures. This is typically implemented either by saving and restoring an
entire state or by keeping track of an *undo trail*, that is, a stack of *undo
actions* that must be performed in order to return to earlier states.

An undo trail presents the following abstract interface:

```
  module Trail () : sig
    val push: (unit -> unit) -> unit
    type checkpoint
    val record: unit -> checkpoint
    val revert: checkpoint -> unit
  end
```

The module `Trail` is in fact a functor: that is, it takes an empty argument
`()`. The functor application `Trail()` initializes a new undo trail, which
is initially empty. This undo trail supports three operations:

* `push action` pushes a new undo action, represented by a function of type
  `unit -> unit`, onto the trail.

  The undo trail has no knowledge of what this undo action does, or what data
  structures it affects. For instance, perhaps the user has just incremented a
  counter `cnt` of type `int ref` and wishes to push an undo action whose
  effect is to decrement `cnt`. The undo trail doesn't need to know; it takes
  care only of executing the right undo actions at the right time.

* `record()` returns the current point in time.
  Such a value is known as a *checkpoint*.

* `revert c` goes back in time to the checkpoint `c`
  by executing *in reverse order* all of the undo actions
  that were pushed after the checkpoint `c` was recorded.

  As a side effect, this invalidates all checkpoints that
  are strictly newer than `c`; these checkpoints must no
  longer be used.

**Question 6.** Implement the module `Trail`.

*Hint.* Maintain a stack of undo actions, stored in an infinite array.

## A Set of Variables

A SAT solver must keep track of a set of *undecided* variables, that is, a
set of variables whose value (`true` or `false`) has not yet been decided.

Recall that a variable is represented as a small integer: indeed,
variables are numbered 0, 1, and so on.

Thus, we need to implement a data structure that represents a set of (small)
integers. This data structure must offer the following abstract interface:

```
  module VarSet () : sig
    val mem: var -> bool
    val add: var -> unit
    val remove: var -> unit
    val pick: unit -> var option
  end
```

The module `VarSet` is in fact a functor: that is, it takes an empty argument
`()`. The functor application `VarSet()` initializes a new set, which is
initially empty. This set supports four operations:

* `mem x` tests whether the variable `x` is currently a member of the set.

* `add x` adds the variable `x` to the set. One can assume that
  `x` initially is not in the set.

* `remove x` removes the variable `x` from the set. One may assume
  that `x` initially is in the set.

* `pick()` picks an arbitrary variable in the set. More precisely,
  if the set is empty, then `pick()` returns `None`. If the set is
  nonempty, then `pick()` returns `Some x`, where `x` is an arbitrary
  element of the set. The variable `x` is *not* removed from the set.

**Question 7.** Implement the module `VarSet`.

*Hint.* Several implementations are possible. We suggest maintaining an
infinite array of Booleans, so as to allow `mem`, `add` and `remove` to
operate in amortized constant time. Some thought is then required to implement
`pick`. There are several ways of doing so; this is up to you!

## Implementing a Simple SAT Solver

At last, we come to the heart of the matter,
that is, implementing a SAT solver.

We have written some code for you,
which sets up four data structures:

* A set `Undecided` of undecided variables.

* An array `C` of all clauses.

* An undo trail `Trail`.

* An array `value` that maps every decided variable to its Boolean value.
  (The Boolean value associated with an undecided variable is irrelevant.)

The array `C.clauses` has type `clause option InfiniteArray.t`. The content of
this array changes over time: indeed, a clause is simplified when a variable
that occurs in it becomes decided. The empty clause, which is unsatisfiable,
is represented by `Some []`. A clause that has been satisfied and therefore
has disappeared is represented by `None`.

The purpose of the undo trail is to help us undo changes to the other data
structures, namely `Undecided` and `C`. (It turns out that it is never
necessary to undo a modification of the array `value`.) This is apparent
in the next two questions.

**Question D.** (Ungraded.) Implement the function `mark_decided` so that
`mark_decided x` takes the variable `x` out of the set `Undecided` and pushes
a suitable undo action onto the trail.

**Question E.** (Ungraded.) Implement the function `set_clause` so that
`set_clause i clause` writes `clause` at index `i` in the array `C.clauses`
and pushes a suitable undo action onto the trail.

As mentioned above, when a variable `x` becomes decided, that is, when the
value of `x` is set to `false` or `true`, we can simplify each of the clauses
where `x` occurs. Every clause where the positive literal `x` appears is now
satisfied and disappears. Every clause where the negative literal `~x` appears
can be simplified by removing this literal. If the clause becomes empty, then
a contradiction has been reached and the exception `UNSAT` must be raised.

**Question F.** (Ungraded.) Implement the function `set_value` so that
`set_value x p` sets the value of `x` to `p`, simplifies all of the
clauses where the variable `x` occurs, and raises `UNSAT` if one
such clause becomes empty.

The reason why the Boolean satisfiability problem is hard is that a huge tree
of candidate solutions must be explored. When a variable `x` is undecided, we
usually have no idea whether `x` should be set to `false` or to `true`, so we
have to explore both branches.

There *is* a situation, however, where it is easy to determine what the
appropriate value of `x` should be. This situation arises when the variable
`x` occurs in a unit clause, that is, a clause that contains just one literal.
If this literal is `x`, then the value of `x` must be `true`.
If this literal is `~x`, then the value of `x` must be `false`.
Exploiting unit clauses allows us to make progress without making arbitrary
decisions; therefore, it is always a profitable activity. This activity is
known as *unit propagation*.

**Question G.** (Ungraded.) Implement the function `find_unit_clause` so that
`find_unit_clause()` finds and returns a unit clause, if there is one. (This
can be done by a naïve linear scan of the array `C.clauses`. The notes at the
end of this exercise suggest improvements.)

**Question H.** (Ungraded.) Implement the function `propagate` so that
`propagate()` performs unit propagation. That is, if there exists a unit
clause `x` or `~x`, mark `x` as decided, set `x` to an appropriate value, and
repeat. (Because `set_value` can simplify existing clauses, exploiting a unit
clause can cause new unit clauses to appear, so the process must be repeated
until there are no unit clauses.)

Once unit propagation is over, if we are lucky, it could be the case that all
variables have been decided, which means that we have found a solution and can
stop. Otherwise, there are no more easy decisions. We must pick a variable
`x`, mark it as decided, and explore (up to) two branches:

* In the first branch, set `x` to (say) `true`, perform unit propagation, and
  continue (recursively).

* If the exception `UNSAT` is raised in the first branch, then setting
  `x` to `true` was a dead end. In that case, we must backtrack, that is,
  restore the state that existed just before entering the first branch,
  and make the opposite decision: set `x` to `false`,
  perform unit propagation, and continue (recursively).

**Question I.** (Ungraded.) Implement the function `explore` so that
`explore()` *either* finds a solution that extends the current partial
assignment of Boolean values to variables and terminates; *or* determines that
there exists no such solution and raises `UNSAT`.

*Hint.* Remember that `explore` assumes that there no are unit clauses, so
`propagate()` must always be called before `explore()`.

We are now almost done. The final task is to write the main function.

**Question 8.** Implement the function `solve`. The arguments of this function
are an integer `n` and a formula `f` whose variables are comprised between 0
included and `n` excluded. If the formula `f` is satisfiable, then `solve`
must return `Some solution`, where the function `solution` maps each variable
in the semi-open interval `[0,n)` to a Boolean value. If the formula `f` is
unsatisfiable, then `solve` must return `None`.

*Hint.* First convert the formula `f` to conjunctive normal form, then run the
SAT solver. Either of these phases can raise the exception `UNSAT`, which must
be handled.

Congratulations: you now have a working SAT solver!

## Notes

There are many ways in which this simplistic SAT solver can be improved.
Here are a few that you can easily experiment with:

* Remove the inefficient linear search in `find_unit_clause`. We want to find
  a unit clause (if one exists) in constant time. This requires setting up
  and maintaining a bag of unit clauses.

* Remove the inefficient linear search in `set_value`. We want to find all of the
  clauses where the variable `x` occurs, in a time that is proportional to the
  number of such clauses. This requires computing (once, at the beginning) a
  list of the clauses where `x` occurs. Better yet, separately pre-compute a
  list of the clauses where `x` occurs positively and a list of the clauses
  where `x` occurs negatively.

* Look for more efficient implementations of the set `Undecided`, and (at the
  same time) think about the strategies that `pick` might implement. For instance,
  it seems desirable to pick a variable that occurs in as many clauses as possible.

The algorithm that we have implemented is a member of the family of
[DPLL algorithms](https://en.wikipedia.org/wiki/DPLL_algorithm).
For a good explanation of these algorithms (in French), accompanied
with simple yet reasonably efficient OCaml implementations, the reader
is referred to the paper
[SAT-MICRO: petit mais costaud!](https://hal.inria.fr/inria-00202831/en/)
by Sylvain Conchon, Johannes Kanig, and Stéphane Lescuyer.

## Infinite Arrays

Conceptually, infinite arrays are exactly like ordinary arrays,
except there is no need to specify their length at creation time:
their length is infinite. The function call `InfiniteArray.make x`
creates a new infinite array and initializes every cell in it with
the value `x`. The cells of this array are indexed by the natural
integers, from zero to infinity. They can be read and written via
the operations `InfiniteArray.get` and `InfiniteArray.set`.

In this exercise, an implementation of infinite arrays is given.
The module `InfiniteArray` offers the following signature:

```
  type 'a t
  val make: 'a -> 'a t
  val get: 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> unit
```

If you are curious how infinite arrays might be implemented,
please study
<a href="" onclick="top.location='/exercises/infinite_arrays/';">this exercise</a>.
