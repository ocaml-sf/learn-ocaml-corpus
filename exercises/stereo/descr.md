# Trees in Stereo Vision

This short exercise considers a simple yet challenging question: *can one
reconstruct a binary tree out of its inorder and preorder traversals?* This
problem was introduced by Knuth in "The Art of Computer Programming" (1968).

## Tree Traversals

Here is an algebraic data type of binary trees:

```
  type 'a tree =
    | Leaf
    | Node of 'a tree * 'a * 'a tree
```

A tree `t` is either a leaf `Leaf` or a binary node `Node (u, x, v)`,
which carries a left subtree `u`, an element `x`, and a right subtree `v`.

A *traversal* of a tree `t` is a list of the elements carried by the tree `t`.
There are several natural ways of ordering such a list. At a binary node `Node
(u, x, v)`, three objects are at hand, namely `u`, `x`, `v`. There are six
permutations of these objects. These six permutations give rise to six natural
traversals, which are known as *inorder*, *preorder*, *postorder*, *reverse
inorder*, *reverse preorder*, and *reverse postorder* traversals. In this
exercise, two of these are of interest:

* The *inorder traversal* corresponds to the permutation `u`, `x`, `v`. It is
  defined as follows: the inorder traversal of a binary node `Node (u, x, v)`
  is formed by concatenating the inorder traversal of the subtree `u`, the
  element `x`, and the inorder traversal of the subtree `v`.

* The *preorder traversal* corresponds to the permutation `x`, `u`, `v`. It is
  defined as follows: the preorder traversal of a binary node `Node (u, x, v)`
  is formed by concatenating the element `x`, the preorder traversal of the
  subtree `u`, and the preorder traversal of the subtree `v`.

**Question 1.** Define two functions `inorder` and `preorder` of type `'a tree
-> 'a list`, such that `inorder t` and `preorder t` are respectively the
inorder and preorder traversals of the tree `t`.

*Note.* The most direct definition of these functions uses list concatenation
`@` and (unfortunately) has quadratic time complexity. There also slightly
more complex definitions that involve an accumulator and have linear time
complexity. You may use whichever definition you prefer.

## Traversal Is Not Injective

In the following, we restrict our attention to trees that do not have
duplicate elements.

The function `inorder` is not injective: it is possible to find two distinct
trees that have the same inorder traversal. The function `preorder` is not
injective either.

**Question 2.** Define two trees `i1` and `i2`, both of type `char tree`, such
that `i1 = i2` is false yet `inorder i1 = inorder i2` is true. The trees `i1`
and `i2` must not have duplicate elements.

**Question 3.** Define two trees `p1` and `p2`, both of type `char tree`, such
that `p1 = p2` is false yet `preorder p1 = preorder p2` is true. The trees `p1`
and `p2` must not have duplicate elements.

Thus, the inorder traversal of a tree, alone, does not contain enough
information to reconstruct this tree. Similarly, the preorder traversal alone
does not allow reconstruction.

Yet, Knuth remarks that if we are given *both* the inorder traversal and the
preorder traversal of a tree, (and these lists have no duplicate elements,)
then we *can* reconstruct this tree. This property is somewhat nonobvious:
perhaps the best way of convincing oneself that it is true is to propose a
reconstruction algorithm.

## Reconstructing a Tree out of its Inorder and Preorder Traversals

Here is the challenge!

**Question 4.** Define a function `reconstruct` of type `char list -> char
  list -> char tree` such that, for every tree `t` (without duplicate
  elements), `reconstruct (inorder t) (preorder t)` yields `t`.
  It is permitted to use OCaml's equality function `=` to test whether two
  elements are equal. It is also permitted to assume that a certain special
  element, say `'$'`, does not appear in the tree `t`.

We have intentionally stated the challenge in a rather abrupt way,
without indicating how to attack it. We now give a number of hints.
Do *not* read all of them straight away! Read them *one by one* and
only after *thinking* for yourself and *trying out* various ideas.

*Hint.* First, try to write a function `reconstruct` of type `char list ->
char tree` that attempts to reconstruct a tree `t` based on its preorder
traversal alone. Although we know that this is impossible (because the
function `preorder` is not injective), it is instructive to try this out and
see where and why it fails. This failed attempt provides a good first step
towards a solution of the challenge.

*Hint.* Think in terms of the goal. The goal is to construct a tree. Thus, we
need to decide whether to construct a leaf or a binary node. In the latter
case, we further need to decide what the root element should be, and we need
to construct the left and right subtrees. What information do we have at hand
in order to guide these decisions?

*Hint.* Here is a much more precise hint. Write an auxiliary function
`reconstruct_aux` of type `char -> char list -> char list -> char tree`,
whose mission is described as follows:

* Assume that the list `xs` begins with the inorder traversal of a tree `t`,
  followed with the element `sentinel`, possibly itself followed with more
  elements `xs'`. Assume that the list `ys` begins with the preorder
  traversal of the tree `t`, possibly followed with more elements `ys'`.
  Assume that the lists `xs` and `ys` have no duplicate elements. Then,
  `reconstruct_aux sentinel xs ys` must return
  the triple `(t, sentinel :: xs', ys')`.

Then, use `reconstruct_aux` to define `reconstruct` in two lines.

## Note

If you have ever written a parser before, this exercise may seem familiar. A
parser is a program that turns a list of symbols into a tree: it is the
inverse function of a tree printer. Here, the exercise consists in writing an
unusual kind of parser, which turns a *pair* of lists of symbols into a tree:
it is the inverse of the function `fun t -> (inorder t, preorder t)`.

To solve this kind of challenge in a systematic manner, some researchers have
advocated the use of *reversible* programming languages, which are restricted
in such a way that every program naturally has an inverse function.
Unfortunately, these programming languages are somewhat difficult to work
with; still, they are at least an interesting curiosity. See for instance
the paper [Constructing a binary tree from its traversals
by reversible recursion and iteration](https://doi.org/10.1016/j.ipl.2019.03.002),
by Glück and Yokoyama (2019), and the references therein.
