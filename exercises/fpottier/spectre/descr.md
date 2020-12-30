# From a Spectre to a Tree

Suppose we have an algebraic data type `'a tree` of binary trees where each
leaf carries a piece of data of type `'a`.

```
  type 'a tree =
    | Leaf of 'a
    | Fork of 'a tree * 'a tree
```

For instance, the tree `Fork (Leaf 'a', Fork (Leaf 'b', Leaf 'c'))`
has type `char tree`, as its leaves carry characters.
The list of the data values carried by its leaves,
read from left to right,
is `['a'; 'b'; 'c']`.
The list of the depths of its leaves,
read from left to right,
is `[1; 2; 2]`.

In fact, these two lists are sufficient to describe this tree in an
unambiguous manner: no other tree has the same list of leaf data and
the same list of leaf depths.

Let us combine these lists, and use the word *spectre* to refer a list
of pairs of a datum and a depth:

```
  type depth =
    int
  type 'a spectre =
    ('a * depth) list
```

The spectre of the tree
`Fork (Leaf 'a', Fork (Leaf 'b', Leaf 'c'))`
is
`[('a', 1); ('b', 2); ('c', 2)]`.

A tree is unambiguously described by its spectre: two distinct trees must have
distinct spectres.

This raises the question: from a spectre, how does one reconstruct a tree?

This simple question gives rise to a nontrivial exercise,
which can be solved in several ways.

This is really a *parsing* problem. Indeed, parsing consists in reconstructing
a tree out of a purely sequential representation of this tree. Furthermore,
because the spectre of a tree carries the depth of each leaf, reconstructing
a tree from a spectre is analogous to *indentation-aware parsing*.

## Warmup: From a Tree to its Spectre

**Question 1.** Define a function `elements` of type `'a tree -> 'a list`
that computes the list of the data values carried by the leaves of a tree.

**Question 2.** Define a function `depths` of type `'a tree -> depth list`
that computes the list of the depths of the leaves of a tree.

**Question 3.** Define a function `spectre` of type `'a tree -> 'a spectre`
that computes the spectre of a tree.

*Note.* The above three questions have naïve solutions that use the list
concatenation operation `@` and (because of that) have quadratic time
complexity. It is possible to avoid the use of concatenation by defining
auxiliary functions that take an accumulator (a list) as an extra argument.
This yields solutions that have linear time complexity.

## Setting up an Input Channel

In preparation for the next question, let us construct a simple notion of
*input channel*.

An input channel helps us read a sequence of things: for instance, a sequence
of bits, a sequence of characters, or (in this exercise) a spectre, that is, a
sequence of pairs of a datum and a depth.

Although we could work with an explicit representation of the input (say, as a
list of things that remain to be read), it is preferable to adopt a more
abstract point of view and to view an *input channel* as an abstract object.
It is useful to think of an input channel as a *read head* that moves along
a fixed *tape*. An input channel supports three operations, namely:

* `peek()` returns `Some x`, where `x` is the element in front of the read
  head, if there is one. It returns `None` if the input has been exhausted,
  that is, if the read head has reached the end of the input.

* `consume()` discards the next input element. In other words, it moves the
  read head forward by one element. Invoking `consume` is permitted only if
  the read head has *not* yet reached the end of the input.

* `current()` indicates how many times `consume` has been called so far. In
  other words, it returns the current position of the read head.

An input channel can be represented as a record of three functions,
as follows:

```
  type position =
    int

  type 'a input =
    {
      peek: unit -> 'a option;
      consume: unit -> unit;
      current: unit -> position;
    }
```

**Question 4.** Define a function `new_input` of type `'a list -> 'a input`
such that `new_input xs` constructs a new input channel whose read head lies
at the beginning of the input sequence `xs`.

*Note.* One could also make `'a input` an abstract type, declare `new_input`
as a toplevel function that returns a new input channel, and declare `peek`,
`consume`, and `current` as three toplevel functions that take an input
channel as an argument. The two approaches are almost equivalent. There is one
slight difference: the record-of-functions approach is more open-ended than
the abstract-type approach, as it allows a user to construct input channels
however she wishes, whereas in the abstract-type approach, the function
`new_input` is the only way of creating an input channel.

## From a Spectre to a Tree

We now reach the main question of this exercise. We state the question first,
then provide extra details and some hints.

**Question 5.** Define a function `build` of type `'a spectre -> 'a tree` that
reconstructs a tree, given its spectre. In other words, for every tree `t`,
the equality `build (spectre t) = t` should hold. If the list `xds` is not a
valid spectre, then `build xds` should raise one of the exceptions presented
below, indicating why and where the input list has been found invalid.

In order to fully understand the above question, one must be aware that not
every list of type `'a spectre` is a valid spectre.

* Some lists are too short: e.g., `[('a', 1)]` is not a valid spectre, because
  no tree has one leaf at depth 1 and no other leaves. The longer list
  `[('a', 1); ('b', 1)]` is a valid spectre.

* Some lists are too long: e.g., `[('a', 0); ('b', 0)]` is not a valid
  spectre, because no tree has two leaves at depth zero. The shorter
  list `[('a', 0)]` is a valid spectre.

* Some lists are ill-formed: e.g., `[('a', 2); ('b', 1); ('c', 2)]`
  is not a valid spectre, as no tree has two leaves at depth 2 with a
  leaf at depth 1 between them.

Thus, we define the following exceptions:

```
  exception InputIsTooShort of position
  exception InputIsTooLong of position
  exception InputIsIllFormed of position
```

We expect the function `build` (see Question 5, above) to raise one of these
exceptions and to provide an appropriate position, indicating exactly where in
the input the problem has been detected. (Positions are numbered 0 and up.)

*Hint.* Regardless of the manner in which you intend to attack the problem, we
recommend converting the input list to an input channel and making this input
channel a parameter of every auxiliary function. Since a spectre is a list of
pairs of a datum and a depth, the type of this input channel must be
`('a * int) input`.

If you would like more suggestions on how to attack the problem, please read
on.

### Grammatical structure of the problem

The main function `build` is expected to build a tree whose root lies at
depth 0. Such a tree is either just a `Leaf` whose depth is 0 or a `Fork`
whose subtrees lie at depth 1. Each of these subtrees itself is either just a
`Leaf` whose depth is 1 or a `Fork` whose subtrees lie at depth 2; and so on.

In general, one can say that a *subtree at depth `d`* is either a `Leaf` whose
depth is `d` or a `Fork` whose children are subtrees at depth `d+1`. One might
write this in Backus-Naur Form (BNF) as follows:

```
  subtree@{d} ::= Leaf (_, d)
                | Fork (subtree@{d+1}, subtree@{d+1})
```

Someone who is familiar with the theory of formal languages might note that
this is very much like a *context-free grammar*. Technically, it is not a
context-free grammar, because it has an infinite number of nonterminal symbols
`subtree@{0}`, `subtree@{1}`, etc. Nevertheless, one can use a number of
well-known approaches to parsing, such as the *top-down approach* and the
*bottom-up* approach, to solve this problem. We describe both; the top-down
approach is easier to understand.

### The top-down approach

The *top-down approach*, also known as the *recursive descent* approach,
consists in writing an auxiliary function `subtree` which takes a depth `d` as
a parameter and is in charge of recognizing and building a *subtree at depth
`d`*, as defined above.

More precisely, the function `subtree` should have type
`int -> ('a * int) input -> 'a tree`. The function call
`depth d input` is expected to consume a portion of the
input that corresponds to a subtree at depth `d` and to
return this subtree.

The main difficulty in the implementation of `subtree` is
to choose between recognizing a `Leaf`, recognizing a
`Fork`, and reporting an ill-formed input. Think about it!

### The bottom-up approach

The *bottom-up* approach consists in working with a list of subtrees, each of
which is accompanied by its depth. Whenever two subtrees at depth `d+1` are
adjacent in the list, they can be combined into a single subtree at depth `d`.
Such a combination step is known as a *reduction*. If the input data is
well-formed, then this process eventually terminates with a list that contains
a single subtree at depth `0`.

Although this idea may seem simple, implementing it in such a way that it
runs in linear time requires some thought.

A bottom-up parser works with a *stack*, a list of pairs of a subtree and its
depth:

```
  type 'a stack =
    ('a tree * int) list
```

The stack should be thought of as a part of the input that has already been
read and partly processed. One should think of it as lying *behind* the read
head, while the remaining input lies *ahead* of the read head. The top of the
stack is its right end, just behind the read head.

A bottom-up parser works as follows:

* Initially, the stack is empty. All of the input lies
  ahead of the read head.

* Under certain conditions, the parser can *shift* the read head, that is,
  move it towards the right. This involves removing one pair `(x, d)` from the
  input and pushing the pair `(Leaf x, d)` onto the stack. Thus, the element
  `x`, which was the first item in front of the read head, is now viewed as a
  subtree that consists of just a leaf, and is now located just behind the
  read head.

* Under certain conditions, the parser can *reduce*, that is, combine the
  topmost two subtrees on the stack. Indeed, if the topmost two subtrees `t`
  and `u` both lie at some depth `d+1`, then they can be replaced on the stack
  with a single tree `Fork (t, u)` at depth `d`. Sometimes, several reduction
  steps can be taken in succession.

* Finally, if at some point the stack consists of a single subtree `t` at
  depth 0 and the read head has reached the end of input, then the parser terminates and returns `t` as its final result.

The main difficulty in the implementation of a bottom-up parser is again to
choose between shifting, reducing, and declaring that the input is ill-formed.

*Hint*. The stack must normally satisfy the following invariant:

* The subtree-depth pairs on the stack, read from the left towards the right,
  have *strictly increasing* depth components.

* If the stack is nonempty, then the topmost subtree-depth pair on the stack
  has a *nonzero* depth component.

A shift action can temporarily break this invariant by pushing onto the stack
a new subtree whose depth is *equal* to the depth of the current topmost
subtree. (It is not allowed to push a new subtree whose depth is *less* than
the depth of the current topmost subtree. Think about it!) In that case, the
invariant can be repaired by performing one or more reduce actions. If, after
these actions, the topmost subtree has depth 0, and if there is no remaining
input, then the parser terminates successfully.

## Notes

The task of reconstructing a tree from its spectre appears as a subroutine in
[Garsia and Wachs' algorithm](https://en.wikipedia.org/wiki/Garsia%E2%80%93Wachs_algorithm), whose purpose is to construct a minimum-cost
binary tree out of a sequence of leaves and their weights. There, the cost of
a tree is defined as the sum over all leaves of the weight of this leaf
multiplied by the depth at which this leaf appears.
[Knuth](https://www-cs-faculty.stanford.edu/~knuth/programs/garsia-wachs.w)
proposes a "quick and dirty" implementation of the Garsia-Wachs algorithm in C
and uses the top-down approach, which he attributes to Tarjan, to reconstruct
a tree from its spectre.
[Filliâtre](https://www.lri.fr/~filliatr/publis/gw-wml08.pdf) implements the
same algorithm in OCaml. [Bird](https://doi.org/10.1017/S0956796819000194)
proposes a different implementation of the Garsia-Wachs algorithm in Haskell
and uses the bottom-up approach to reconstruct a tree from its spectre.

The top-down approach and the bottom-up approach are close cousins of the
parsing techniques known in the literature as
[LL(1)](https://en.wikipedia.org/wiki/LL_parser) and
[LR(1)](https://en.wikipedia.org/wiki/LR_parser), respectively.

Parsing a list of elements annotated with their depth is related to
*indentation-aware parsing*, where the amount of whitespace found at the
beginning of each line influences the parser. Adams' paper
[Principled Parsing for Indentation-Sensitive
Languages](https://michaeldadams.org/papers/layout_parsing/LayoutParsing.pdf)
describes an indentation-aware extension of LR(1).
