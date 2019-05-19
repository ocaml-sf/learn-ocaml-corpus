# The Nonattacking Queens Problem

The Nonattacking Queens problem,
also known as the N-Queens problem,
consists in placing `n` queens on a chess board of size `n` by `n`,
while respecting the constraint that **no two queens can threaten each other:**
that is, no two queens can lie on the same column, row, or diagonal.
We say that a placement of `n` queens on the board is **safe** when this
constraint is respected.

We are interested in counting and enumerating the solutions to this problem.
This is a classic exercise in nondeterministic programming. To hide the
details of backtracking search, **we use an implementation of the
nondeterminism monad**, which is given to us.

A simple-minded approach to the Nonattacking Queens problem consists in
**generating** all possible placements of `n` queens on the board and
**testing**, for each candidate placement, whether it is safe.

A more efficient approach requires **pruning** the search space, by testing
**partial placements** for safety, and eliminating unsafe partial placements
as early as possible.

## The Nondeterminism Monad

When searching for the solution of a problem, one must typically
explore multiple choices. If a series of choices lead to a failure
(a dead end), then one must backtrack and explore another avenue.

There are a number of ways in which nondeterminism and backtracking
can be implemented. Regardless of which implementation mechanism is
chosen, it is desirable to hide it behind an abstraction barrier and
present the end user with **a simple API for constructing an executing
nondeterministic computations**.

This API is known as the **nondeterminism monad**. It offers the
following key elements:

* A type `'a m`, the type of computations that yield results of type
 `'a`.

* A number of constructor functions for constructing computations,
  such as `fail: 'a m`, which represents failure, and `choose: 'a m
  -> 'a m -> 'a m`, which expresses a nondeterministic choice
  between two computations.

* A single observation function, `sols: 'a m -> 'a Seq.t`, which
  converts a computation to a sequence of results, thereby allowing
  the user to execute this computation and observe its results. The
  name `sols` stands for `solutions`.

A monad can be thought of as a **mini-programming language** where
computations are first-class citizens: we have a type of
computations, ways of building computations, and a way of executing
computations.

A computation in the nondeterminism monad can produce zero, one, or
more results. Indeed, a computation that fails produces zero
results. A computation that succeeds normally produces one result.
A computation that uses `choose` can produce more than one result.
It is in fact possible to construct computations that produce an
infinite number of results!

## The Nondeterminism Monad's API

The signature, or API, of the nondeterminism monad is as follows:

```
  (* Type. *)
  type 'a m

  (* Constructor functions. *)
  val return: 'a -> 'a m
  val (>>=): 'a m -> ('a -> 'b m) -> 'b m
  val fail: 'a m
  val choose: 'a m -> 'a m -> 'a m
  val delay: (unit -> 'a m) -> 'a m

  (* Observation function. *)
  val sols: 'a m -> 'a Seq.t
```

As explained above, a value of type `'a m` is **a description of a
computation**, which, once executed, produces a sequence of results
of type `'a`.

To execute a computation `m`, one must first convert it to a
sequence of type `'a Seq.t`, whose elements can then be demanded,
one by one. (More information on the module `Seq` is given below.)
This conversion is performed by the observation function `sols`.

The call `sols m` typically terminates in constant time; the actual
computation described by `m` takes place only when the elements of
the sequence `sols m` are demanded, and only insofar as necessary to
produce the elements that are demanded. For instance, applying
`Seq.head` to the sequence `sols m` forces the computation to
proceed up to the point where it is able to produce its first
result.

The constructor functions `return` and `(>>=)` exist in all monads.
(They are also known as `return` and `bind`.) `return` constructs a
trivial computation, which does nothing except return a value,
whereas `(>>=)` constructs the sequential composition of two
computations. Together, they allow constructing the sequential
composition of an arbitrary number of computations.

* The computation `return v` succeeds exactly once with the value
  `v`. In other words, the sequence of values that it produces is
  the singleton sequence composed of just `v`.

* The computation `m1 >>= m2` is the sequential composition of the
  computations `m1` and `m2`. This composition operator is
  asymmetric: whereas its first argument `m1` is a computation of
  type `'a m`, its second argument `m2` is a function of type `a ->
  'b m`. Every value `x` produced by `m1` is passed to `m2`,
  yielding a computation `m2 x`. The sequence of values produced by
  `m1 >>= m2` is the concatenation of the sequences of values
  produced by the computations `m2 x`, where `x` ranges over the
  values produced by `m1`.

The constructor functions `fail` and `choose` are specific of the
nondeterminism monad. `fail` can be thought of as a 0-ary
disjunction, whereas `choose` is a binary disjunction. Together,
they allow constructing the disjunction of an arbitrary number of
computations.

* The computation `fail` returns no result. In other words, it
  produces an empty sequence of values.

* The sequence of values produced by `choose m1 m2` is the
  concatenation of the sequences of values produced by `m1`
  and by `m2`.

The constructor function `delay` is used to delay the construction
of a computation until the moment where this computation must be
executed. Indeed, a difficulty that arises in a strict programming
language, such as OCaml, is that the arguments passed to constructor
functions, such as `return` and `choose`, are evaluated immediately,
at construction time. For instance, when one writes `choose e1 e2`,
both of the OCaml expressions `e1` and `e2` are evaluated before
`choose` is invoked. If the evaluation of `e2` performs nontrivial
work, then this work arguably takes place too early: indeed, there
should be no need to evaluate `e2` until all of the values produced
by `e1` have been demanded. To remedy this, one may write `choose e1
(delay (fun () -> e2))`. There, the expression `e2` is placed in the
body of an anonymous function, so `e2` is not evaluated immediately.
This anonymous function, whose type is `unit -> 'a m`,
is converted by `delay` to a computation of type `'a m`.
This conversion requires no serious work;
it is performed in constant time.
An intuitive reason why this is possible is that the type `'a m`
represents a *suspended* computation already,
so the type `unit -> 'a m` represents a *suspended suspended*
computation, which is essentially the same thing; these types
are interconvertible at no cost.

The computations `e` and `delay (fun () -> e)` produce the same
sequence of results. The only difference between them is the time at
which the evaluation of `e` takes place: either immediately, or only
when the first result is demanded.

It is worth noting that in a lazy language, such as Haskell, there
is no need for `delay`. In such a language, when one writes `choose
e1 e2`, the expressions `e1` and `e2` are *not* evaluated
immediately: they are evaluated only when their value is demanded.
Thus, the fact that `e2` need not be evaluated until all of the
values produced by `e1` have been demanded goes without saying. The
fact that there is no need for explicit uses of `delay` is arguably
a strength of lazy languages. At the same time, the fact that it is
not obvious where laziness plays a crucial role is arguably a
weakness of lazy languages. In OCaml, in contrast, the explicit use
of `delay` can be verbose, but helps understand what is going on.

## The `Seq` API

The type of **on-demand sequences** is defined in a module named `Seq`.
Beginning with version 4.07,
this module is part of OCaml's standard library.

```
module Seq : sig

  type 'a t = unit -> 'a node

  and +'a node =
  | Nil
  | Cons of 'a * 'a t

  val nil : 'a t
  val cons: 'a -> 'a t -> 'a t
  val singleton: 'a -> 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val concat: 'a t -> 'a t -> 'a t
  val flatten: 'a t t -> 'a t

  val take: int -> 'a t -> 'a t

  val head: 'a t -> 'a option

  val of_list: 'a list -> 'a t
  val to_list: 'a t -> 'a list

end
```

This data type is closely related to the algebraic data type of lists.
Indeed, if instead of `unit -> 'a node` one had written just `'a node`,
then this data type would have been isomorphic to the type of lists.

The presence of `unit -> ...` indicates that a sequence is in fact a function.
Calling this function, by applying it to the value `()`, amounts to requesting
the head of the sequence. This head can be either `Nil`, which means that the
sequence is empty, or `Cons (x, xs)`, which means that the first element of
the sequence is `x` and the remaining elements form another sequence `xs`. It
is worth noting that `xs` is itself a function, so the elements of the
sequence `xs` need not be explicitly computed until `xs` is applied.

Sequences are closely related to *iterators* in object-oriented languages,
such as C++ and Java. Yet, sequences are much simpler than iterators, for
two reasons:

* they involve no mutable state;

* they are just as easy to construct and to use as ordinary lists.

The functions `nil`, `cons`, and `singleton` are constructor functions.

The functions `map`, `concat`, `flatten` are analogues for sequences
of the standard list functions `List.map`, `(@)`, and
`List.flatten`.

The function `Seq.take` truncates a sequence at a certain length:
`Seq.take n xs` is a sequence that begins like `xs` but has at most
`n` elements.

The function `Seq.head` demands the first element of a sequence. If
the sequence begins with an element `x`, then `Some x` is returned;
otherwise, `None` is returned. This forces enough computation to
take place so as to be able to produce the first element of the
sequence.

The functions `Seq.of_list` and `Seq.to_list` convert between lists
and sequences, both ways. One must keep in mind that applying
`Seq.to_list` to a sequence `xs` causes all of its elements to be
demanded: that is, it forces all of the suspended computations to
take place. In particular, if `xs` is an infinite sequence, then
`Seq.to_list xs` does not terminate.

## Selecting an Element from a List

As a first exercise in nondeterministic programming, we wish to define
a computation `select xs` which chooses an arbitrary element `x` from
the list `xs` and returns a pair of this element and the list deprived
of this element.

**Question 1.**
Define a function `select` of type `'a list -> ('a * 'a list) m`.

*Hint.* Be careful. The function call `select xs` itself should run in
constant time, and should construct a computation which (when executed)
produces as many results as there are elements in the list `xs`. It is
necessary to use `delay`.

## Exploring a Tree of Choices

Another common exercise in nondeterministic programming is to write code that
searches a space of partial candidate solutions to a problem. Such a space is
typically *tree-structured*. Indeed, at each state in the search space, one the
following situations arises:

* The current state represents a complete candidate solution.
  This candidate is ready to be tested,
  so as to determine whether it is indeed a valid solution.
  This is a *leaf* in the search tree.

* The current state represents an incomplete (partial) candidate solution.
  This partial solution must then be extended.
  There are typically several ways of doing so,
  so a choice must be made.
  This is a *node* in the search tree:
  it can have zero, one, or more children.

Because we work in the nondeterminism monad, we can in fact often forget about
this tree structure and pretend to descend along just *one branch* of the
tree: at each node, we make a choice, descend into one child, and continue.
This view shift is the key idea that simplifies the way in which we think
about our search algorithm.

We now wish to write a function `unfold` which descends into one branch
and constructs a list of the choices that are made along the way.

```
val unfold: ('s -> bool) -> ('s -> ('a * 's) m) -> 's -> 'a list m
```

The idea is as follows. We are exploring a search space whose states have type
`'s`, and as we go, we are making choices that can be described by values of
type `'a`. The function `unfold` takes three arguments:

* A function `final` of type `'s -> bool`.
  When applied to a state `s`, this function
  determines whether this state is a leaf.

* A function `step` of type `'s -> ('a * 's) m`.
  When applied to a non-final state `s`,
  this function constructs a nondeterministic computation
  whose results are pairs `(x, s')`
  of a choice `x`
  and the new state `s'` that is reached by making this choice.

* An initial state `s`, the root of the search tree.

The function call `unfold final step s` builds a nondeterministic computation
whose results are lists of choices. More precisely, each result is a list
`[ x_0; x_1; ...; x_{n-1} ]`, which describes a sequence of consecutive
choices, beginning with the least recent choice `x_0` and ending with the most
recent choice `x_{n-1}`. Such a list describes one branch in the search tree,
that is, the path to one leaf in the search tree. The order in which results
are produced does not matter.

The computation is iterative: it can be thought of as a loop. It begins in
state `s`. At each iteration, if the current state is *not* final, then the
function `step` is used to make a choice and move to a new state, and the loop
continues. If the current state is final, then the loop ends, and the sequence
of all choices that were made along the way is returned.

**Question 2.** Implement `unfold`.

*Note.* The automatic tests for Question 2 involve the functions `is_zero`,
`decrement`, and `decrease`, which are defined in the Prelude. If your code
does not pass the tests, you will see an error message that mentions some of
these functions.

## Enumerating the Permutations of a List

By building on top of your work in Questions 1 and 2,
it is now very easy to solve the following problem.

**Question 3.** Write a function `permutations`
of type `'a list -> 'a list m`
such that the nondeterministic computation `permutations xs`
produces all permutations of the list `xs`.

## The Nonattacking Queens Problem

A placement of queens on the board can be described as a set of points
`(i, j)`, where `i` is a column and `j` is a row.

Because two queens cannot occupy the same column, two points in this set
cannot have the same first coordinate `i`. This implies that a placement is
actually a partial *function* of rows to columns.

Because two queens cannot occupy the same row, two points in this set cannot
have the same second coordinate `j`. This implies that a placement is actually
an *injective* partial function of rows to columns.

Therefore, a complete placement is
an injective function of the interval `[0, n)` into itself.
In other words, it is a *permutation* of the interval `[0, n)`.

A partial placement, where `k` queens have been placed in the `k` leftmost
columns, is an injective function of `[0, k)` into `[0, n)`.

By convention, we represent such a partial placement as a list of length `k`
whose elements are distinct integers in the range `[0, n)`.

```
type placement =
  int list
```

Because two queens cannot occupy the same diagonal,
two points in a **safe** placement cannot have
the same sum `i + j` or the same difference `i - j`.

**Question 4.** Write a function `nodup` of type `'a list -> bool`
such that `nodup xs` returns `true` if and only if the list `xs`
does *not* have duplicate elements. It is permitted to use OCaml's
generic functions for equality `(=)`, disequality `(<>)`, and
ordering (`compare`) at type `'a`.

*Hint.* First, write an auxiliary function `nocdup` that tests for
*consecutive* duplicate elements in a list. Then, implement `nodup`
as the composition of sorting and `nocdup`.

**Question 5.** Write a function `safe` of type `placement -> bool`
which determines whether a (partial or complete) placement is safe.

*Hint.* First, compute a list of the sums `i + j`, where `(i, j)`
ranges over the coordinates of each queen in the placement. (Use
`List.mapi`.) Then, verify that this list has no duplicate elements.
Proceed analogously with the list of the differences `i - j`.

**Question 6.** Write a function `queens` of type `int -> placement m`
such that the computation `queens n` produces *all safe placements* of
`n` queens on a chess board of size `n` by `n`.

## Pruning Early

Questions 1-6 form a simple-minded approach to the Nonattacking Queens
problem. We **generate** all possible placements of `n` queens on the
board and **test**, for each candidate placement, whether it is safe.
When `n` is 7, for instance, this involves enumerating 7! placements,
that is, 5040 permutations, of which only 40 are safe.

A more efficient approach requires **pruning** the search space, by testing
**partial placements** for safety, and eliminating unsafe partial placements
as early as possible.

To do so, we can keep using `unfold`, which explores a tree of choices, but
we must modify our `step` function so that an unsafe partial placement has
no children. This means that `step` must test whether the current partial
placement is safe, and if it is *not* safe, fail. We may also enrich the type
of states so as to make this test cheaper.

**Question 7.** Write a function `queenz` of type `int -> placement m`
such that the computation `queenz n` produces *all safe placements* of
`n` queens on a chess board of size `n` by `n`. Write the code in such
a way that unsafe partial placements are detected as early as possible.

*Hints.* We suggest proceeding as follows:

* Define a module `IntSet` that offers an implementation of sets of integers.
  (Use the standard library functor `Set.Make`.)

* Define a function `occupy` of type `int -> IntSet.t -> IntSet.t m`
  such that `occupy x xs` fails if `x` is already a member of the set `xs`
  and returns the union of the sets `{x}` and `xs` otherwise.

* Define the type `state` to be a quadruple `(sums, differences, i, js)`
  where:
  - `sums`, a set of integers, is the set of the sums `i + j`,
    where `(i, j)` ranges over the coordinates of each queen
    that has been placed already;
  - `differences`, a set of integers, is the set of the differences `i - j`,
    where `(i, j)` ranges over the coordinates of each queen
    that has been placed already;
  - `i`, an integer, is the column in which the next queen should be placed;
  - `js`, a list of integers, is the list of rows in which no queen has been
    placed yet.

* Define a function `initial` of type `int -> state`
  such that `initial n` is an appropriate initial state.

* Define a function `final` of type `int -> state -> bool`
  such that `final n` determines whether a state is final,
  that is, whether the placement is complete.

* Define a function `step` of type `state -> (int * state) m`
  such that the computation `step s`,
  starting from a (safe, nonfinal) state `s`,
  enumerates all possible ways of
  extending the current partial placement with one new queen,
  while ensuring that the extended placement remains safe.
  This computation should return pairs `(j, s')`,
  where `j` is the column in which the new queen is placed
  and `s'` is the new state.

* Define `queenz` in terms of `unfold`, `final`, `step`, and `initial`.

## Trivia

The number of ways of placing `n` nonattacking queens on a chess board of size `n` by `n`
is documented as [sequence A000170](https://oeis.org/A000170) in the
Online Encyclopedia of Integer Sequences.
The sequence begins like this:

```
  1, 1, 0, 0, 2, 10, 4, 40, 92, 352, 724, 2680, 14200, 73712, 365596, 2279184, 14772512, ...
```

How far can you go?
