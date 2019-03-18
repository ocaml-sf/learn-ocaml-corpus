# Tree Iterators

In this exercise, we study how to **produce (on demand) a sequence of the
elements of a data structure**. We use a simple binary-tree data structure as
a representative example, but our technique is applicable to any data
structure.

We use the type `'a Seq.t` of **on-demand sequences** to represent a producer
of a sequence of elements. The main point that we wish to make is that **this
type is very similar to the type of lists**, `'a list`, so implementing a
producer of a sequence of elements requires essentially the same programming
skill as building a list of elements.

## Binary trees

A type `'a tree` of binary trees is given, where every binary node is
decorated with an *element*, that is, a piece of data of type `'a`.
This type is defined as follows:

```
type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree
```

One could impose invariants on this data structure, including an ordering
invariant and a balancing invariant, to obtain *balanced binary search trees*,
a simple, widely-known implementation of dictionaries. In this exercise,
however, these invariants play no role.

The **fringe** of a tree is the list of its elements, in the order in which
they appear when the tree is written down as an OCaml expression. (This is
sometimes known as an infix enumeration.) For instance, the fringe of the tree
`Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf))` is the list
`[0; 1; 2]`.

## Building the fringe of a tree (eagerly)

**Question 1.**
Define a function `slow_elements`
of type `'a tree -> 'a list`
such that `slow_elements t` is the fringe of the tree `t`.
Give a direct recursive definition of this function,
aiming for simplicity, not for efficiency.

When Question 1 above is answered in the most straightforward way,
the solution involves the list concatenation function `(@)`, whose
time complexity is linear in the length of its first argument.
As a result, the complexity of the function `slow_elements` is bad:
it is `O(n^2)` on arbitrary trees,
and `O(n log n)` on weight-balanced trees.
Yet, ideally, traversing a tree should take only linear time.

A classic solution to this problem is to
write a function `elements_with`
that takes a **continuation list** `ys` as
a second argument. Each tree element is
consed (that is, prepended) to `ys`,
thereby eliminating the need for list concatenation.

**Question 2.**
Define a function `elements_with`
of type `'a tree -> 'a list -> 'a list`
such that the list `elements_with t ys` is
equal to the list `slow_elements t @ ys`.

**Question 3.**
In one line,
define a function `elements`
of type `'a tree -> 'a list`
such that `elements t` is the fringe of the tree `t`.
(Thus, the lists `elements t` and `slow_elements t` are equal.)

The function `elements` obtained in Question 3 builds the fringe of a tree in
linear time, which is good. A problem remains, however: suppose that someone
needs access to just the first `k` elements of the fringe, but the value of
`k` is not known ahead of time. In such a scenario, building the entire fringe
in memory is wasteful. We would like to be able to produce elements **on
demand**, with `O(1)` work per element, so that the first `k` elements are
produced in time `O(k)`.

Fortunately, there is a very simple way of doing so. The trick is to use a
slightly more elaborate, yet closely related data structure: we replace
*lists* with *on-demand sequences*.

## On-demand sequences

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

  exception Trap
  val trap: 'a t

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

The functions `nil` and `cons` are constructor functions. They are provided
for convenience, but could just as well be defined outside of the module
`Seq`. Their definitions (not shown) are one line long.

The sequence `trap` behaves in a special way: when it is applied to `()`,
which means that its head is requested, it raises the exception `Trap`.
You will not need to use `trap`.
The automatic grading code for Question 5 uses it internally.

## Building the fringe of a tree (on demand)

We now wish to adapt the code written in Questions 2 and 3
so as to produce a sequence instead of a list.

**Question 4.**
Define two mutually recursive functions
`fringe_seq_with` and
`fringe_node_with`,
whose types are as follows:
```
  val fringe_seq_with:  'a tree -> 'a Seq.t -> 'a Seq.t
  val fringe_node_with: 'a tree -> 'a Seq.t -> 'a Seq.node
```
such that the sequence `fringe_seq_with t ys`
begins with the fringe of the tree `t`
and continues with the sequence `ys`.
Then, in one line,
define a function `fringe`
of type `'a tree -> 'a Seq.t`
such that `fringe t` is the fringe of the tree `t`.

## Testing the equality of two sequences

**Question 5.**
Define a function `equal`
of type `'a Seq.t -> 'a Seq.t -> bool`
such that `equal xs ys` is `true`
if and only if
the sequences `xs` and `ys` are equal.
The sequences `xs` and `ys` are assumed to be finite.
Furthermore, we assume that it is permitted to use
the generic equality function `(=)` to
compare two elements of type `'a`.

*Note.* If the sequences `xs` and `ys` differ at some point, say at their
`i`-th elements, then they must not be evaluated further than that; that is,
the `i+1`-th element of either sequence must never be requested.

## Testing whether two trees have the same fringe

**Question 6.**
Define a function `same_fringe`
of type `'a tree -> 'a tree -> bool`
such that `same_fringe t1 t2` is `true`
if and only if the trees `t1` and `t2`
have the same fringe.

Of course, if the trees `t1` and `t2` have distinct fringes,
then the comparison should stop and return `false` as early
as possible.

This exercise has real-world applications. For instance, the function
`same_fringe` can be used to test whether two sets, represented as binary
search trees, have the same elements.

This is sometimes known in the literature as the *same-fringe problem*.
It can be a difficult problem if attacked naively.
Thanks to the tools developed in this exercise,
it is quite straightforward.
