# Implementing Nondeterminism with Sequences

In this exercise, we build an implementation of the nondeterminism
monad. This monad admits several possible implementations; the one
that we choose here is a direct implementation where a computation
is represented as an on-demand sequence of values.

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

Thus, a useful way to think of a computation is as **a sequence of
results**, that is, a sequence of results.

Because of this remark, one might be tempted to define the type `'a
m` as a synonym for `'a Seq.t`, the type of sequences of values of
type `'a`. However, although it is possible to represent a
computation internally as a sequence (and we will do so in this
exercise), this is not necessarily the best implementation
technique. Thus, we prefer to make the API more flexible by viewing
`'a m` as an abstract type and by offering an observation function,
`sols`, which converts a computation to a sequence.

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
  val at_most_once: 'a m -> 'a m
  val interleave: 'a m -> 'a m -> 'a m
  val (>>-): 'a m -> ('a -> 'b m) -> 'b m

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

The constructor function `at_most_once` constructs a computation
that succeeds at most once. If `m` fails, then `at_most_once m`
fails as well. If `m` produces a result `x`, possibly followed with
more results, then `at_most_once m` produces just the result `x`,
and no more. This constructor can be used to commit to a result and
prevent any other choices from being explored. In other words, it
limits the amount of backtracking that takes place.

The constructor function `interleave` has the same type as `choose`.
It is a *fair disjunction* operator. An ordinary disjunction `choose
m1 m2` gives priority to its left branch: it first lets `e1` produce
as many results as it wishes, then gives control to `e2`. This can
be problematic: if `e1` produces a large number of results, then
`e2` is tried very late. At an extreme, if `e1` produces an infinite
number of results, then `e2` is never tried. For instance, supposing
that `evens` produces the infinite sequence `0, 2, 4, ...` and
`odds` produces the infinite sequence `1, 3, 5, ...`,
the disjunction `choose evens odds` is equivalent to just `evens`,
which seems counter-intuitive and undesirable. In contrast,
`interleave` is defined in such a way that `interleave evens odd`
produces the infinite sequence `0, 1, 2, 3, 4, 5, ...`.
It is *fair* in the sense that each branch in turn is allowed
to produce a result.

The constructor function `(>>-)` has the same type as `(>>=)`.
It is a *fair sequencing* operator.
Indeed, a problem with ordinary sequencing `(>>=)` is that
it gives rise to ordinary (unfair) disjunctions.
To see this, suppose that the left-hand argument of `(>>=)`
is a computation that produces `x` as its first result,
followed with a computation `m1` that may produce more results.
Thus, this left-hand argument is equivalent to
`choose (return x) m1`.
When we sequentially compose it with a computation `m2`,
we obtain
`(choose (return x) m1) >>= m2`,
which is equivalent to
`choose (return x >>= m2) (m1 >>= m2)`,
which itself is the same as
`choose (m2 x) (m1 >>= m2)`.
We are faced with an ordinary (unfair) disjunction.
The problem, again, is that if `m2 x` produces an
infinite number of results, then `m1 >>= m2`
is never executed.
To remedy this problem,
the fair sequencing operator `(>>-)`
is defined in such a way that
`choose (return x) m1 >>- m2`
is equivalent to
`interleave (m2 x) (m1 >>- m2)`.
Thus, it gives rise to a fair disjunction.

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

## Implementing the Nondeterminism Monad

In this exercise, we **implement** the nondeterminism monad by
representing a computation as a sequence of results.
Thus, we adopt the following type definition:

```
  type 'a m = 'a Seq.t
```

This is the simplest possible manner of implementing computations.
Indeed, we wrote above that a computation **produces** a sequence
of results. Here, we decide that a computation **is** a sequence
of results.

**Question 1.** Implement the four constructor functions
`return`, `(>>=)`, `fail`, and `choose`. Implement the
observation function `sols`.

*Hint.* Think of the sequence of results that the computations
`return x`, `m1 >>= m2`, `fail`, and `choose m1 m2` are supposed to
produce. Then, use the facilities provided by the module `Seq` to
build these sequences. Each of these five functions can be
implemented in one very short line.

*Note.* The automated grading system first tests whether your code
is functionally correct. To do so, it builds a computation using the
four constructor functions, converts it to a sequence via `sols`,
and tests whether this produces the expected sequence of results.
Then, it tests whether your code is lazy, that is, whether each
result is computed as late as possible. To do so, it uses a
constructor function `tick: 'a m -> 'a m` whose effect is as
follows: the computation `tick m` produces the same sequence of
results as the computation `m`, and, when executed, increments a
global counter named `work`. Thus, by executing computations that
contain `tick`s, the automated grading system can tell when
computations are executed. For instance, demanding just the first
result of the computation
`choose (tick (return 0)) (tick (return 1))` should cause `work` to
be incremented just once, not twice.

**Question 2.** Implement the constructor function `at_most_once`.

**Question 3.** Implement the constructor function `interleave`.

*Hint.* As a first step, you may first implement
`interleave: 'a list -> 'a list -> 'a list`
as an operation on lists. Perform case analysis
on the first argument: either it is empty, or
it is nonempty.

*Hint.* Then, adapt your code so that `interleave` has the desired
type `'a m -> 'a m -> 'a m`. The constructor function `delay` can be
useful.

**Question 4.** Implement the constructor function `(>>-)`.

*Hint.* Again, perform case analysis on the first argument.

## Notes

This exercise is based on the paper
**"Deriving backtracking monad transformers"**
[(Hinze, 2000)](https://www.cs.ox.ac.uk/ralf.hinze/publications/ICFP00.ps.gz)
and on the first part of the paper
**"Backtracking, Interleaving, and Terminating Monad Transformers"**
[(Kiselyov, Shan, Friedman, and Sabry, 2005)](http://okmij.org/ftp/papers/LogicT.pdf)
(up to section 4.2).

It is important to note that these papers use Haskell, a lazy programming
language, where computations are suspended by default. Thus, where they use
lists, we cannot use OCaml lists; we must instead use OCaml sequences.
Furthermore, we must sometimes be careful to explicitly delay computations:
our `delay` constructor serves this purpose.

These papers go beyond us by implementing nondeterminism not just as a monad,
but as a **monad transformer**. Monad transformers can be composed. For
instance, by composing the nondeterminism monad transformer and the state
monad transformer, one can describe computations that involve both
nondeterminism and mutable state.

Implementing nondeterministic computations directly as Haskell lists or as
OCaml sequences, as we do in this exercise, is the simplest implementation. It
is in agreement with our intuitive understanding of computations: indeed, we
like to think of a nondeterministic computation as a process that produces a
sequence of results.

Unfortunately, this implementation is inefficient: because `choose` is
implemented as sequence concatenation, the cost of a single use of `choose` is
linear in the number of results produced by the left-hand side. In other
words, in a binary tree of `choose` constructors, transmitting a result from a
left child to its father costs O(1). In the worst case, executing the left-leaning
computation `choose (choose (choose (... (choose 0 1) ...) (n-2)) (n-1)) n`
has quadratic cost O(n^2), even though it produces only O(n) results.

In a
<a href="" onclick="top.location='/exercises/nondet_monad_cont/';">companion exercise</a>,
we switch to a more efficient implementation,
based on the use of success and failure **continuations**.
