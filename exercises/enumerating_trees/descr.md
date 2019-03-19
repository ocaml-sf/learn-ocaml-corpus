# Enumerating Trees

In this exercise, we write code that can **count**, **enumerate**,
and **sample** the elements of a certain data type.
This allows us to do things such as:

* Count the binary trees of size `s`.
* Iterate over all binary trees of size `s`.
* Pick a binary tree of size `s` uniformly at random among all such trees.

We will also be interested in imposing additional invariants: for instance, we
can easily restrict our attention to **balanced** binary trees.

This exercise is inspired by the paper
[Feat: functional enumeration of algebraic types](https://www.semanticscholar.org/paper/Feat%3A-functional-enumeration-of-algebraic-types-Dureg%C3%A5rd-Jansson/46aabcca02c47095b29f934e8d7198bf4c58c27d)
by Jonas Duregård, Patrik Jansson and Meng Wang.

## Enumerations of Sized Data

Instead of restricting our attention to the type of trees (as in the above
examples), we would like to work with an arbitrary type `'a` whose values are
equipped with a notion of **size**. The type `'a` may have an infinite number
of values; yet, we assume that size is defined in such a way that, for every
natural number `s`, there is a finite subset of values of type `'a` whose size
is `s`. We would like to be able to count, enumerate, and sample this subset.

With this in mind, we define an **enumeration** of the type `'a`
to be a function which, given a *nonnegative* size `s`,
returns a finite sequence of values of type `'a` whose size is `s`.

```
  type 'a enum =
    int -> 'a Seq.seq
```

Thus, an enumeration `e` of type `'a enum` represents a set of elements of
type `'a`, grouped by size. The function call `e(s)` returns the subset of
the elements of size `s`.
(This subset is represented as a sequence without duplicate elements.)
The function `e` is allowed to assume that its argument `s` is nonnegative;
an enumeration must never be passed a negative size.

The module `Seq`, which is given to you, implements an abstract type of
sequences of elements. Its interface is described below.
Its implementation is the topic of other exercises, entitled
"Symbolic Sequences as Objects" and
"Symbolic Sequences as Data".

## The `Seq` API

`'a seq` is the type of a sequence whose elements have type `'a`.
```
  type 'a seq
```

### Constructors

There are five constructor functions:
`empty`,
`singleton`,
`sum`,
`product`,
and `map`.

Each of these functions has **constant time** complexity.
Indeed, they do not actually construct the entire sequence in memory;
they construct only a compact representation of it.

The constant `empty` is the empty sequence.

```
  val empty: 'a seq
```

The sequence `singleton x` has length 1. Its single element is `x`.

```
  val singleton: 'a -> 'a seq
```

The sequence `sum s1 s2` is the concatenation of the sequences `s1` and `s2`.

```
  val sum: 'a seq -> 'a seq -> 'a seq
```

The sequence `bigsum ss` is the concatenation of all sequences in the list
`ss`.

```
  val bigsum: 'a seq list -> 'a seq
```

The sequence `product s1 s2`, a sequence of pairs, is the Cartesian product
of the sequences `s1` and `s2`. Its length is the product of the lengths of
`s1` and `s2`. The first pair component is considered most significant,
which means that the product of the integer sequence `0; 1` by itself is the
sequence `(0, 0); (0, 1); (1, 0); (1, 1)`.

```
  val product: 'a seq -> 'b seq -> ('a * 'b) seq
```

The sequence `map phi s` is the image of the sequence `s` through the function
`phi`.

```
  val map: ('a -> 'b) -> 'a seq -> 'b seq
```

### Destructors

There are three destructors: `length`, `get`, and `foreach`.

`length s` returns the length of the sequence `s`.

```
  val length: 'a seq -> int
```

If the index `i` is comprised between zero (included) and `length s` (excluded),
then `get s i` returns the `i`-th element of the sequence `s`.
Otherwise, `get s i` raises the exception `OutOfBounds`.

```
  exception OutOfBounds
  val get: 'a seq -> int -> 'a
```

`foreach s k` iterates over all elements of the sequence `s`.
Each element in turn is passed to the loop body `k`.
Thus, the OCaml code `foreach s (fun x -> ...)` can be
informally understood as `for each x in s do ...`.

```
  val foreach: 'a seq -> ('a -> unit) -> unit
```

`elements s` returns a list of all elements of the sequence `s`.
In principle, you don't need to use it,
but it can be useful for debugging.

```
  val elements: 'a seq -> 'a list
```

## Constructing enumerations

We begin by defining six basic functions that construct enumerations.

**Question 1.** Define a constant `empty` of type `'a enum` which represents
the empty set.

**Question 2.** Define a function `just` of type `'a -> 'a enum` such that
`just x` represents a singleton set, whose element is `x`, and the size of
this element is considered to be zero.

**Question 3.** Define a function `pay` of type `'a enum -> 'a enum` such that
the enumeration `pay e` represents the same set as the enumeration `e`, but
the size of every element is increased by one. That is, if a value `x` appears
in `e` among the elements of size `s`, then it should appear in `pay e` among
the elements of size `s+1`.

*Note*. `pay` is so named because the elements of the enumeration `pay e` are
*more expensive*, so to speak, than the elements of the enumeration `e`.

**Question 4.** Define a function `sum` of type `'a enum -> 'a enum -> 'a
enum` such that `sum e1 e2` represents the union of the sets `e1` and `e2`.
One can assume that the sets `e1` and `e2` are disjoint.

**Question 5.** Define a function `product` of type
`'a enum -> 'b enum -> ('a * 'b) enum`
such that `product e1 e2` represents the Cartesian product
of the sets `e1` and `e2`.
By convention, the size of a pair `(x1, x2)` is the sum of the sizes
of `x1` and `x2`.

*Hint.* A pair `(x1, x2)` has size `s` if and only if
`x1` has size `s1` and `x2` has size `s2`
*for some* integers `s1` and `s2` such that `s1 + s2 = s` holds.

*Hint.* Define an auxiliary function `up: int -> int -> int list`
such that `up i j` is the list `[i; i+1; ...; j]`.
Use the functions `Seq.bigsum`, `List.map`, and `Seq.product` so
as to combine all of the possibilities that arise out of
all valid choices of `s1` and `s2`.

**Question 6.** Define a function `map` of type
`('a -> 'b) -> 'a enum -> 'b enum`
such that the set `map f e` is the image through `f` of the set `e`.
By convention, the size of the element `f x` in the enumeration `map f e`
is the size of the element `x` in the enumeration `e`.

## Enumerating lists

In the following, we use the constructor functions defined above.

**Question 7.** Define an enumeration `bit` of type `int enum`
whose values, `0` and `1`, are both considered to have size 0.

We assume that a
[memoizing](https://en.wikipedia.org/wiki/Memoization)
fixed point combinator `fix` is given. Its type is
`(('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)`. The idea is, instead of defining
an ordinary recursive function by `let rec f x = e`, a user can define a
memoizing recursive function by `let f = fix (fun f x -> e)`.
The implementation of `fix` uses a hash table;
there is an implicit assumption that
OCaml's generic equality and hash functions
can be used at type `'a`.
For more details, see the exercise "Counting Trees".

It is worth noting that, because an enumeration is a function,
`fix` also has type
`('a enum -> 'a enum) -> 'a enum`
as a special case.
Thus, `fix` can be used to recursively define an enumeration.

**Question 8.** Define a function `list` of type `'a enum -> 'a list enum` such
that, if `e` is an enumeration of a certain set of elements, then `list e` is
an enumeration of the lists of elements drawn from `e`. By convention, the
size of a list is *the sum of its length and of the sizes of its elements*.

*Hint.* The function `list` *itself* need not be recursive or memoized. (In
fact, it cannot be memoized using `fix`, as its domain type `'a enum` does
not support generic equality and hashing.) However, the value *returned* by
the call `list e`, which has type `'a list enum`, is also a function. This
function must be recursive, and, for better time and space efficiency, it
should be memoized.

*Hint.* In order to guarantee termination, every recursive call should appear
under a `pay` constructor. (Why is this so? Think about it.)

*Suggestion.* After answering these questions, go to the Toplevel pane and
evaluate the expression `Seq.elements (list bit 3)`, which yields a list of
all lists of bits of length 3. (Because, by convention, a bit has size 0, the
size of a list of bits is the length of the list.)

## Enumerating trees

A binary tree is either a leaf,
which carries no children,
or a binary node,
which carries two children:

```
type tree =
  | Leaf
  | Node of tree * tree
```

A tree carries no data; we are interested purely in its shape,
not in the data that it might carry.

This definition is the same as in the exercise "Counting Trees".

**Question 9.** Define an enumeration `tree` of type `tree enum` such
that `tree` is an enumeration of all binary trees. By convention, the
size of a tree is the total number of its binary nodes.

*Hint.* The function `tree` must be recursive. For better time and space
efficiency, it should be memoized. In order to guarantee termination, every
recursive call should appear under a `pay` constructor.

*Suggestion.* After answering this question, go to the Toplevel pane and
evaluate the expression `Seq.elements (tree 3)`, which yields a list of all
trees of size 3.

```
# Seq.elements (tree 3);;
- : tree list =
[Node (Leaf, Node (Leaf, Node (Leaf, Leaf)));
 Node (Leaf, Node (Node (Leaf, Leaf), Leaf));
 Node (Node (Leaf, Leaf), Node (Leaf, Leaf));
 Node (Node (Leaf, Node (Leaf, Leaf)), Leaf);
 Node (Node (Node (Leaf, Leaf), Leaf), Leaf)]
```

## Enumerating balanced trees

The property of being weight-balanced, or **balanced** for short,
is inductively defined as follows.
An empty tree `Leaf` is balanced.
A nonempty tree `Node(t1, t2)` is balanced if
its children `t1` and `t2` are balanced and
their sizes differ by at most one.

We would now like to construct an enumeration of the balanced
binary trees.

To do so, we first define a variant of the `product` combinator, named
`balanced_product`, which enumerates only balanced pairs, as opposed to all pairs.
It is then straightforward to enumerate the balanced trees.

**Question 10.** Define a function `balanced_product` of type
`'a enum -> 'b enum -> ('a * 'b) enum`
such that `balanced_product e1 e2` represents
the set of all pairs `(x1, x2)` such that
`x1` is drawn from `e1`,
`x2` is drawn from `e2`,
and the sizes of `x1` and `x2` differ by at most one.
By convention, the size of a pair `(x1, x2)` is the sum of the sizes
of `x1` and `x2`.

*Hint.* When asked to produce a sequence of pairs of size `s`,
consider two cases: either `s` is even, or it is odd. In each
case, under the constraint that the pair `(x1, x2)` is balanced and has size `s`,
what are the possible sizes of `x1` and `x2`?

**Question 11.** Define an enumeration `balanced_tree` of type `tree enum`
such that `balanced_tree` is an enumeration of all balanced binary trees.

*Suggestion.* After answering this question, go to the Toplevel pane and
check that there are 4 balanced trees of size 4:

```
# Seq.elements (balanced_tree 4);;
- : tree list =
[Node (Node (Leaf, Leaf), Node (Leaf, Node (Leaf, Leaf)));
 Node (Node (Leaf, Leaf), Node (Node (Leaf, Leaf), Leaf));
 Node (Node (Leaf, Node (Leaf, Leaf)), Node (Leaf, Leaf));
 Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf))]
```

and there is only one balanced tree of size 7:

```
# Seq.elements (balanced_tree 7);;
- : tree list =
[Node (Node (Node (Leaf, Leaf), Node (Leaf, Leaf)),
  Node (Node (Leaf, Leaf), Node (Leaf, Leaf)))]
```

## Enumerating balanced tidy labeled trees

As a final twist, let us consider a slightly richer type of **labeled trees**,
where every leaf carries an integer datum:

```
type labeled_tree =
  | LLeaf of int
  | LNode of labeled_tree * labeled_tree
```

The **fringe** of a labeled tree is the sequence of integers
found at its leaves, listed from from left to right.

A labeled tree of size `s` is **tidy** if its fringe is the sequence
`0; 1; ... ; s-1`.

**Question 12.** Define an enumeration `balanced_tidy_tree` of type
`labeled_tree enum` such that `balanced_tidy_tree` is an enumeration
of all balanced tidy labeled trees.

*Hint.* First write a function `label` of type `tree -> labeled_tree`
which converts a tree to a tidy labeled tree of the same shape. Then,
apply this transformation to the enumeration `balanced_tree` defined
in Question 11.

*Suggestion.* After answering this question, go to the Toplevel pane and
admire the 4 balanced tidy labeled trees of size 4:

```
# Seq.elements (balanced_tidy_tree 4);;
- : labeled_tree list =
[LNode (LNode (LLeaf 0, LLeaf 1), LNode (LLeaf 2, LNode (LLeaf 3, LLeaf 4)));
 LNode (LNode (LLeaf 0, LLeaf 1), LNode (LNode (LLeaf 2, LLeaf 3), LLeaf 4));
 LNode (LNode (LLeaf 0, LNode (LLeaf 1, LLeaf 2)), LNode (LLeaf 3, LLeaf 4));
 LNode (LNode (LNode (LLeaf 0, LLeaf 1), LLeaf 2), LNode (LLeaf 3, LLeaf 4))]
```
