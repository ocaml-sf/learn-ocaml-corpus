# Implementing Nondeterminism as an Abstract Machine

In this exercise, we build an implementation of the nondeterminism
monad. This monad admits several possible implementations. The one
that we choose here can be described as an *abstract machine* that
interprets a piece of *code* and uses two auxiliary stack-like data
structures, namely a *success continuation*, and a *failure continuation*.
Each of these three data structures is described by a generalized
algebraic data type.

This implementation can be viewed as a defunctionalized version
of the continuation-based implementation studied in
<a href="" onclick="top.location='/exercises/nondet_monad_cont/';">another exercise</a>.
The underlying algorithm is exactly the same,
but whereas the continuation-based implementation represents
code and continuations as first-class functions,
this implementation represents them as data structures.

We suggest doing the other exercise first
and attempting this exercise next.

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
and no more. This combinator can be used to commit to a result and
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

## Data Structures for Continuations and Computations

In this exercise, we implement an abstract machine that manipulates three data
structures: a **computation**, a **failure continuation**, and a **success
continuation**. The idea is, in general, the computation indicates what to do
next; in the case where the computation is `fail`, the failure continuation
indicates what to do next; in the case where the computation is `return x`,
the success continuation indicates what to do next.

Each of these three data structures is described by a generalized algebraic
data type. We now give the definitions of these data types. (These definitions
can in fact be *deduced* from the solution of the
<a href="" onclick="top.location='/exercises/nondet_monad_cont/';">companion exercise</a>,
by defunctionalization, but we leave this story for another time.)

The type `'a m` of **computations** is defined as follows:

```
type 'a m =
| MDelay:               (unit -> 'a m) -> 'a m
| MReturn:                          'a -> 'a m
| MBind:           'a m * ('a -> 'b m) -> 'b m
| MFail:                                  'a m
| MChoose:                 'a m * 'a m -> 'a m
| MReflect: ('a * 'a m) option failure -> 'a m
```

The data constructors `MDelay`, `MReturn`, `MBind`, `MFail`, and `MChoose`
correspond directly to the constructor functions `delay`, `return`, `>>=`,
`fail`, and `choose`. In other words, a computation is represented internally
by its syntax.

If `f` is a failure continuation whose answer type is `('a * 'a m) option`,
then `MReflect f` is a computation. In other words, the data constructor
`MReflect` allows disguising such a failure continuation as a computation.
This is useful in the implementation of the operation `msplit`, which
itself is used in Question 2-4, as in the
<a href="" onclick="top.location='/exercises/nondet_monad_cont/';">companion exercise</a>.

The type `'answer failure` of **failure continuations** is defined as follows:

```
and 'answer failure =
| FChoose:
    (* m2: *) 'a m *
    (* s : *) ('a, 'answer) success *
    (* f : *) 'answer failure ->
              'answer failure
| FSols:
    _ Seq.node failure
| FSplit:
    _ option failure
```

Thus, a failure continuation is a data structure. This data structure can be
*interpreted* as a function of type `unit -> 'answer` by the function
`apply_failure`, whose type is
`'answer failure -> unit -> 'answer`,
and whose code you have to complete as part of this exercise.
There are three data constructors:

* `FChoose (m2, s, f)` records the fact that a choice of the form `choose m1
  m2` has been entered. The right-hand branch `m2` as well as the current
  success and failure continuations `s` and `f` are recorded, so as to allow
  the computation to continue if and after the left-hand branch `m1` fails.

* `FSols` records the fact that this computation is executed by the
  observation function `sols`. When interpreted as a function, this
  failure continuation returns `Seq.Nil`.

* `FSplit` records the fact that this computation is executed by the
  observation function `msplit`. When interpreted as a function, this
  failure continuation returns `None`.

The type `('a, 'answer) success` of **success continuations** is defined
as follows:

```
and ('a, 'answer) success =
| SBind:
    (* m2: *) ('a -> 'b m) *
    (* s : *) ('b, 'answer) success ->
              ('a, 'answer) success
| SSols:
    ('a, 'a Seq.node) success
| SSplit:
    ('a, ('a * 'a m) option) success
```

Thus, a success continuation is a data structure. This data structure can be
*interpreted* as a function of type `'a -> 'answer failure -> 'answer`
by the function `apply_success`,
whose type is `('a, 'answer) success -> 'a -> 'answer failure -> 'answer`,
and whose code you have to complete as part of this exercise.
There are three data constructors:

* `SBind (m2, s)` records the fact that a sequence of the form `m1 >>= m2` has
  been entered. The right-hand side of the sequence `m2` as well as the
  current success continuation `s` are recorded so as to allow the computation
  to continue if (and every time) `m1` produces a result.

* `SSols` records the fact that this computation is executed by the
  observation function `sols`. When interpreted as a function, this
  success continuation returns an answer of the form `Seq.Cons (_, _)`.

* `SSplit` records the fact that this computation is executed by the
  observation function `msplit`. When interpreted as a function, this
  success continuation returns an answer of the form `Some (_, _)`.

The data types `'a m`, `'answer failure` and `('a, 'answer) success` are
mutually recursive. Thus, the three functions which interpret these data
types, namely `compute`, `apply_failure`, and `apply_success`, are
mutually recursive as well.

## Implementing the Abstract Machine

The four constructor functions `return`, `(>>=)`, `fail`, and `choose`
have been implemented for you already. They are trivial anyway.

**Question 1.**
Complete the definitions of the functions
`apply_failure`, `apply_success`, and `compute`.
Then, implement the observation function `sols`.

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

*Note.* The automated grading system for Question 1 does not exercise the
cases for `FSplit`, `SSplit` and `MReflect` in the abstract machine. In order
to ensure that your code deals with these cases correctly, you must continue
this exercise and pass Questions 2-4 as well.

As in the continuation-based implementation described in a
<a href="" onclick="top.location='/exercises/nondet_monad_cont/';">companion exercise</a>,
before attacking Questions 2-4,
we must define an auxiliary function, `msplit`.

The need for `msplit` arises from the following remark. When executed, a
computation of type `'a m` produces a sequence of results. Such a sequence
must be either empty or nonempty, and in the latter case, it must have a first
element and a remainder. If a computation *was* just a sequence (that is, if
the type `'a m` was a synonym for `'a Seq.t`), then we could distinguish these
two situations just by performing a case analysis.

Because we have defined the type `'a m` in a different way, a direct case
analysis is impossible. Instead, the idea is to define a function `msplit`
that *converts* from the type `'a m` to the type `unit -> ('a * 'a m) option`.
This opens the door to a case analysis. In short, `msplit` allows us to
perform case analysis on a computation *as if* it was a sequence, even though
its internal representation is different.

**Question X.** (Not graded.)
Complete the implementation of the auxiliary function `msplit.`

Once `msplit` is available,
Questions 2-4 are answered in exactly the same way as in the
<a href="" onclick="top.location='/exercises/nondet_monad_cont/';">companion exercise</a>.

**Question 2.** Implement the constructor function `at_most_once`.

*Hint.* Use `msplit` to determine if the computation at hand fails or produces
at least one result. In the first case, recall that `at_most_once fail` is
supposed to behave like `fail`. In the second case, recall that
`at_most_once (choose (return x) m)` is supposed to behave like
`return x`.

*Hint.* Use `delay` to ensure that the case analysis is performed when
the computation `at_most_once m` is *executed*, not when it is *built*.

**Question 3.** Implement the constructor function `interleave`.

*Hint.* Again, use `delay` and `msplit` in an appropriate manner.
Then, ask yourself how `interleave fail m2` is supposed to behave,
and how `interleave (choose (return x1) m1) m2` is supposed
to behave.

**Question 4.** Implement the constructor function `(>>-)`.

*Hint.* Again, use `delay` and `msplit` in an appropriate manner.
Then, ask yourself how `fail >>- m2` is supposed to behave,
and how `(choose (return x1) m1) >>- m2` is supposed
to behave.

## Notes

This implementation can be obtained from the continuation-based implementation studied in
<a href="" onclick="top.location='/exercises/nondet_monad_cont/';">another exercise</a>
in a systematic manner by performing
[defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization).
This is in fact arguably a rather natural way of discovering this
implementation, as it could otherwise be difficult to *guess* the
definitions of the algebraic data types `_ failure`,
`(_, _) success`, and `_ m`.
A paper by Pottier and Gauthier
explains how defunctionalization
gives rise to a generalized algebraic data type
and an `apply` function
in the transformed program.
Here, the process gives rise to three generalized algebraic data types
and three `apply` functions, because we defunctionalize computations,
success continuations, and failure continuations independently.
