# Generic Sorting

**Sorting** is the problem of rearranging an input sequence according to a
given total preorder.

It is well-known that, in order to sort a sequence of `n` elements, a
**comparison-based** sorting algorithm needs `Omega(n log n)` comparisons,
that is, at least `k * n * log n` comparisons, for a certain constant factor
`k`.

Comparison-based sorting algorithms are popular because they are
**polymorphic** in the type of elements. That is, **they do not and cannot
inspect** the structure of elements; they have access only to a user-supplied
comparison function.

That said, not every sorting algorithm is comparison-based. For certain types
of elements and for certain preorders, other techniques may be available.
**[Pigeonhole sort](https://en.wikipedia.org/wiki/Pigeonhole_sort)**,
for instance, sorts a sequence of (bounded) integers in time
`O(n)`. Furthermore, there are constructions by which **linear-time
sorting algorithms can be composed**: for instance, if there is a linear-time
algorithm that sorts values of type `'k1` according to a preorder `o1`, and if
there is a linear-time algorithm that sorts values of type `'k2` according to
a preorder `o2`, then it is not very difficult to construct a linear-time
algorithm that sorts **pairs** of type `'k1 * 'k2` according to the
lexicographic ordering `o1 * o2`.

These remarks suggest that there is **a family of preorders** for which
one can construct a linear-time sorting algorithm.

In this exercise, we exhibit such a family of preorders. It takes the form of
an algebraic data type `'k order`. A value `o` of type `'k order` is a
**description** of a total preorder on values of type `'k`.
For every such value `o`, we are able to give a linear-time sorting algorithm.
This claim is substantiated by defining a function `sort` whose type is as follows:

```
  val sort: 'k order -> ('k * 'v) list -> 'v list
```

The function `sort o` accepts a list of key-value pairs,
sorts it according to the preorder `o`,
which is a preorder on keys,
and returns just a list of values.
The keys are dropped.
(This somewhat unusual presentation, where keys and values are distinguished,
helps define `sort` recursively. A more standard presentation, where keys and
values are identified, can easily be obtained as a special case; see Question 4.)

The time complexity of `sort o kvs` is `O(n)`, where `n` is the length of the
input list `kvs`, and where the constant factor is allowed to depend on the
preorder `o`.

Throughout this exercise, we restrict our attention to **stable** sorting
algorithms. That is, if two values are associated with equivalent keys
according to the preorder `o`, then they must appear in the output list in the
same order as in the input list.

This exercise is inspired by the paper
[Generic top-down discrimination for sorting and partitioning in linear
time](https://www.cs.ox.ac.uk/projects/utgp/school/henglein2012c.pdf)
(Henglein, 2012).
A related paper is
[Sorting and searching by distribution: from generic discrimination to
generic tries](http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/31/slides/aplas2013.pdf)
(Henglein and Hinze, 2013).

The algorithms considered here and in these papers are *generic*. Here, this
word means that these algorithms are polymorphic and directed by an explicit
description of the data at hand. Indeed, they can be applied to an arbitrary
type `'k` of keys, provided a description `o` of a preorder on keys, of type
`'k order`, is provided.

## Warmup: Pigeonhole Sort

**Question 1.** Define a function `pigeonhole_sort`
of type `int -> (int * 'v) list -> 'v list`
such that `pigeonhole_sort bound kvs`
sorts the list of key-value pairs `kvs`
and returns just a list of values.
The keys are integers
and must be comprised between `0` (included) and `bound` (excluded).
The time complexity of the algorithm must be `O(bound + n)`,
where `n` is the length of the list `kvs`.
This must be a stable sort.

*Hint.* Build an array, indexed by keys,
where each array cell holds a list of values.
Convert this array back to a sorted list.

*Note.* The automatic grading code verifies whether the output list is
correct, but does not verify the time complexity of your algorithm. It
is up to you to convince yourself that the complexity bound is met.

*Note.* In a real-world implementation,
one would use memoization so as to ensure
that two successive calls to `pigeonhole_sort bound`
use the same internal array,
instead of allocating a new array every time.

## A family of preorders

As announced earlier, in this exercise, we exhibit a family of preorders
for which one can construct a linear-time sorting algorithm.

This family takes the form of an algebraic data type `'k order`. A value `o`
of type `'k order` is a **description** of a total preorder on values of type
`'k`. We use the letter `'k` because we typically refer to these values as
**keys**.

The type `'k order` is defined as follows.
(This is in fact a [generalized algebraic data type](http://caml.inria.fr/pub/docs/manual-ocaml/extn.html).)

```
  type _ order =
  | OTrue:                                         'k order
  | ONat:                                  int -> int order
  | OSum : 'k1 order * 'k2 order -> ('k1, 'k2) either order
  | OProd:       'k1 order * 'k2 order -> ('k1 * 'k2) order
  | OMap :            ('k1 -> 'k2) * 'k2 order -> 'k1 order
```

Let us review the meaning of each of these data constructors.
This explanation may seem a bit lengthy,
but such is the cost of precision!

* `OTrue` represents the **everywhere-true** preorder.
  According to this preorder, all keys are equivalent.
  By itself, this preorder is not very useful.
  It typically serves as a basic component in the construction
  of more complex composite preorders (see Questions 5 to 8).

* `ONat bound` represents the **standard total order on the integers**
  comprised between `0` (included) and `bound` (excluded).

* `OSum (o1, o2)` represents the **disjoint union** of the preorders `o1` and `o2`.

  More precisely,
  if `o1` represents a total preorder on values of type `'k1`
  and if `o2` represents a total preorder on values of type `'k2`,
  then `OSum (o1, o2)` represents a total preorder on values of type
  `('k1, 'k2) either`,
  defined as follows:
  - `Inl v1` is less than `Inr v2`;
  - `Inl v1` is less than `Inl w1` if and only if
    `v1` is less than `w1` according to `o1`;
  - `Inr v2` is less than `Inr w2` if and only if
    `v2` is less than `w2` according to `o2`.

* `OProd (o1, o2)` represents the
  **[lexicographic product](https://en.wikipedia.org/wiki/Lexicographic_product_of_graphs)**
  of the preorders `o1` and `o2`.

  More precisely,
  if `o1` represents a total preorder on values of type `'k1`
  and if `o2` represents a total preorder on values of type `'k2`,
  then `OProd (o1, o2)` represents a total preorder on values of type
  `'k1 * 'k2`,
  defined as follows:
  - If `v1` is strictly less than `w1` according to `o1`,
    then `(v1, v2)` is less than `(w1, w2)`;
  - If `v1` is equivalent to `w1` according to `o1`,
    and if `v2` is less than `w2` according to `o2`,
    then `(v1, v2)` is less than `(w1, w2)`.

* `OMap (f, o2)` is the **inverse image** of the preorder `o2`
  through the function `f`.

  More precisely,
  if `f` has type `'k1 -> 'k2`
  and if `o2` represents a total preorder on values of type `'k2`,
  then `OMap (f, o2)` represents a total preorder on values of type `'k1`,
  defined as follows:
  - `v1` is less than `w1`
    if and only if `f(v1)` is less than `f(w1)` according to `o2`.

The above explanation can be translated into executable code:

**Question 2.**
Define a function `cmp` of type
`'k order -> 'k -> 'k -> result`,
where the type `result`, which describes the outcome of a comparison,
is defined by `type result = Lt | Eq | Gt`.
The function `cmp` should be an interpreter:
that is, if `o` is a description of a certain preorder,
then `cmp o` must be a decision function for this preorder.

## Sorting

It is now time to prove that
for every preorder `o`,
there is a linear-time sorting algorithm.

**Question 3.**
Define a function `sort`
of type `'k order -> ('k * 'v) list -> 'v list`
such that `sort o` accepts a list of key-value pairs,
sorts it according to the preorder `o`,
and returns just a list of values,
dropping the keys.
This must be a stable sort.
The time complexity of `sort o kvs` must be `O(n)`, where `n` is the length
of the input list `kvs`, and where the constant factor is allowed to depend on
the preorder `o`.

*Note.* The automatic grading code verifies whether the output list is
correct, but does not verify the time complexity of your algorithm. It
is up to you to convince yourself that the complexity bound is met.

Sometimes the key-value distinction is not useful:
one simply wishes to sort a list of values.
This is easily done:

**Question 4.**
In one line,
define a function `simple_sort`
of type `'v order -> 'v list -> 'v list`
such that `sort o` accepts a list of values
and sorts it according to the preorder `o`.

## User-defined preorders

The five primitive preorder constructors,
`OTrue`, `ONat`, `OSum`, `OProd`, and `OMap`,
are sufficiently expressive
for many preorders of practical interest
to be definable.

**Question 5.** Define a value `bool`
of type `bool order`
which represents the standard total order on the Booleans.

**Question 6.** Define a value `list`
of type `'a order -> 'a list order`,
such that
if `o` is a total preorder on letters of type `'a`
then
`list o` is the corresponding
[lexicographic preorder](https://en.wikipedia.org/wiki/Lexicographical_order)
on sequences of letters.

*Hint.* A list is either empty or a pair of an element and a list. In other
words, the type `'a list` is isomorphic to the type `(unit, 'a * 'a list)
either`. As a first step, define a function `unfold` of type
`'a list -> (unit, 'a * 'a list) either` that witnesses one
direction of this isomorphism. Then, use `unfold` in the definition
of the lexicographic preorder `list`.

*Hint.* OCaml allows recursive definitions of *values*. For instance, the
definition `let rec ones = 1 :: ones` gives rise to an infinite list,
represented in memory as a cyclic data structure. This feature can be
exploited in the definition of the lexicographic preorder `list o`.

**Question 7.** Define a value `string`
of type `string order`
which represents the standard lexicographic order
on strings.

*Hint.* An OCaml string is isomorphic to a sequence of characters,
and an OCaml character is isomorphic to an 8-bit integer.

The function `sort`, when applied to the preorder `ONat bound`,
uses `pigeonhole_sort`,
which allocates and initializes an array of size `bound`.
When `bound` is small (say, 256), this is acceptable;
when `bound` is large, this becomes untenable.

Fortunately, a large integer can be cut down into a tuple of smaller integers!
This is the idea behind **[radix sort](https://en.wikipedia.org/wiki/Radix_sort)**.

**Question 8.** Define a value `int32`
of type `int order`
which represents the standard order on integers
comprised between `0` (included) and `2^32` (excluded).
This definition must be such that `sort int32`
internally allocates arrays of size `256`, no more.

## Discrimination

For every preorder `o`,
one can define not just a linear-time sorting algorithm,
but also a linear-time **discrimination** algorithm.

Discrimination goes a little bit beyond sorting:
in addition to sorting a list of key-value pairs,
a discrimination algorithm also groups together
those values that are associated with equivalent keys.

Thus, instead of producing just a list of values,
it produces a list of groups,
where each group is a *nonempty* list of values.

**Question 9.**
Define a function `discr`
of type `'k order -> ('k * 'v) list -> 'v list list`
such that `discr o` accepts a list of key-value pairs
and returns a list of groups,
where each group is a nonempty list of values that are associated with equivalent keys,
and the groups are sorted by key according to the preorder `o`.
