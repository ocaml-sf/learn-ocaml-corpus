# Counting trees

In this exercise, we write code that counts binary trees.
This allows us to answer questions such as:

* How many binary trees have **weight** `w`?
* How many binary trees have **height** `h`?
* How many **weight-balanced** binary trees have weight `w`?
* How many **height-balanced** binary trees have height `h`?

This is an opportunity to think about
**counting** the inhabitants of a type,
a process that is related to
**enumerating** the inhabitants of a type,
albeit counting is slightly simpler than enumerating.

This is also an opportunity to use **dynamic programming**
or **memoization** in order to share repeated subcomputations
and avoid exponential time complexity.

For simplicity, we use OCaml's built-in limited-precision integers.
As we compute fast-growing sequences, we are quickly in danger
of computing incorrect results due to overflow.
To escape this limitation, we would have to
use a library such as [GNU MP](https://gmplib.org/) or [BigNum](https://opam.ocaml.org/packages/bignum/).

## Trees

A binary tree is either a leaf,
which carries no children,
or a binary node,
which carries two children:

```
type tree =
  | Leaf
  | Node of tree * tree
```

Here, a tree carries no data; we are interested purely in its **shape**,
not in the data that it might carry.

## Weight and height

The **weight** of a tree is the total number of its binary nodes.

The **height** of a tree is the maximum number of binary nodes
that appear along a branch.

**Question 1.** Define a function `weight` of type `tree -> int`
which computes the weight of a tree.

**Question 2.** Define a function `height` of type `tree -> int`
which computes the height of a tree.

## Counting trees via a direct, recursive approach

How many (distinct) trees of 5 nodes are there? The answer is 42.
More generally, if `w` is an arbitrary nonnegative integer,
how many trees of weight `w` are there?

**Question 3.** Define a recursive function `naive_trees_of_weight` of type
`int -> int` such that `naive_trees_of_weight w` is the number of
trees of weight `w`. If needed, read the hints that follow.

*Hint*. If a tree has positive weight `w`, then it must be of the form `Node
(t1, t2)`, and the sizes `w1` and `w2` of its subtrees `t1` and `t2` must
satisfy `w1 + w2 + 1 = w`.

*Hint*. If there are `n1` possibles shapes for the left subtree `t1` and `n2`
possible shapes for the right subtree `t2`, then there are `n1 * n2` possible
shapes for the tree `Node (t1, t2)`.

One problem with this recursive formulation is that it is very slow. Because
some subcomputations are repeated over and over again, it has exponential time
complexity.

## Counting trees by dynamic programming

In order to avoid repeated subcomputations,
a classic technique is **dynamic programming**.

The idea is to decide in advance which computations will take place
and in what order they will take place.

Here, we wish to compute
the number of trees of weight 0,
the number of trees of weight 1,
and so on,
in this order,
until we obtain the number of trees of weight `w`,
for some `w` of interest.
While computing the number of trees of weight `j`,
we may need the number of trees of weight `i`,
where `i < j` holds. All we have to do,
in that case, is look up this information in a table,
since it has been computed already.

**Question 4.** Define a nonrecursive function `trees_of_weights` of type
`int -> int array` such that `trees_of_weights w` is an array of size `w+1`
whose entry at index `w` is the number of trees of weight `w`.

Dynamic programming is an interesting misnomer: it could instead be called
**static scheduling**, since the order in which subcomputations take place is
decided ahead of time.

It is a powerful technique, but is slightly inconvenient, as one must
explicitly decide the order in which computations take place and encode it
using one or more loops.

Furthermore, in some cases, dynamic programming can perform useless work.
Indeed, every computation in the schedule is performed exactly once. However,
some computations that appear in the schedule may turn out to not influence
the computation of the final result.

## Memoization

To address the above issues, one can exploit **memoization**. In this
approach, we write a recursive function, as in our initial naive approach, but
we arrange for the results of every call (including intermediate recursive
calls) to be recorded in a table. Thus, if a call is repeated, then its result
is immediately found in the table.

In this approach, each computation is performed at most once, possibly never.
The order in which computations take place remains implicit.

Memoization can be implemented once and for all as a function `fix`. The idea
is, instead of defining an ordinary recursive function by `let rec f x = ...`,
a user can define a memoizing recursive function by `let f = fix (fun f x -> ...)`.

**Question 5.** Complete the definition of the function `fix`, whose skeleton
is given to you.

## Counting trees using a memoizing recursive function

We now come back to our initial motivation, which is to answer the question:
if `w` is an arbitrary nonnegative integer, how many trees of weight `w` are
there?

In order to express the answer to this question in an elegant manner, let us
first write two auxiliary functions, `sigma` and `split_weight`.

In the following, a **weight** (written `w`, `w1`, `w2`, etc.) is always
nonnegative.

**Question 6.** Define a function `sigma` of type `int -> int -> (int -> int)
  -> int` such that `sigma i j f` is the sum `f i + f (i+1) + ... + f j`. The
sum is zero if `j` is less than `i`.

**Question 7.** Define a function `split_weight` of type `int -> (int -> int
-> int) -> int` such that `split_weight w f` is the sum of all terms of the
form `f w1 w2`, where `w1` and `w2` must obey the constraint `w1 + w2 = w`.

**Question 8.** Define a memoizing function `trees_of_weight` of type
`int -> int` such that `trees_of_weight w` is the number of
trees of weight `w`.

**Question 9.** Construct a list `trees_of_weight_0_19` of the values of
`trees_of_weight w`, where `w` ranges from 0 to 19 (included).

**Hint.** First define a recursive function `tabulate` of type `(int -> 'a) ->
int -> int -> 'a list` such that `tabulate f i j` is a list of the values `f
x`, where `x` ranges from `i` to `j` (included).

The numbers in this sequence are known as the
[Catalan numbers](https://oeis.org/A000108).

## Counting weight-balanced trees

The property of being **weight-balanced** is inductively defined as follows.
An empty tree `Leaf` is weight-balanced.
A nonempty tree `Node(t1, t2)` is weight-balanced if
its children `t1` and `t2` are weight-balanced and
their weights differ by at most one.

**Question 10.** Define a function `split_wb_weight` of type `int -> (int ->
int -> int) -> int` such that `split_wb_weight w f` is the sum of all terms of
the form `f w1 w2`, where `w1` and `w2` must obey the constraint `w1 + w2 = w`
and must differ by at most one.

**Question 11.** Define a memoizing function `wb_trees_of_weight` of type `int
-> int` such that `wb_trees_of_weight w` is the number of weight-balanced
trees of weight `w`.

**Question 12.** Construct a list `wb_trees_of_weight_0_19` of the values of
`wb_trees_of_weight w`, where `w` ranges from 0 to 19 (included).

This is quite [an amazing sequence](https://oeis.org/A110316), isn't it?
