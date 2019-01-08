# Leftist heaps

This problem is inspired by Chapter 3 of Chris Okasaki's book,
[Purely Functional Data Structures](https://www.cambridge.org/fr/academic/subjects/computer-science/programming-languages-and-applied-logic/purely-functional-data-structures?format=PB&isbn=9780521663502).

## Mission statement

We assume that there is a type `element` of elements
and a function `priority` of type `element -> int`
whose definitions are not revealed.

```
type element = ... (* abstract *)
type priority = int
let priority (x : element) : priority = ... (* abstract *)
```

Thus, each element carries an integer priority.
By convention, a smaller priority denotes a "more urgent" element.

We wish to implement a **priority queue**, that is, a multiset (also known as
a bag) of elements which supports the following operations: `empty`,
`singleton`, `insert`, `union`, and `extract`, which extracts an element whose
priority is minimal.

## Data structure

The data structure that we wish to use is known as **leftist heaps**.

A heap `h` is an immutable binary tree. It is either an empty heap `E` or a
nonempty heap `T (r, x, h1, h2)` where `r` is an integer rank, `x` is an
element, and `h1` and `h2` are subheaps.

```
type rank = int
type heap =
  | E
  | T of rank * element * heap * heap
```

Because this data structure is immutable,
it is automatically persistent: the operations
that we are about to implement are nondestructive.

A heap must satisfy the **heap invariant**: if an element `x` is
stored higher in the tree than an element `y`, then `x` is more urgent than
`y`, that is, `priority x <= priority y` holds. In particular, the element
that is stored at the root of the tree has minimal priority.

By definition, the **rank** of a heap is the length of its right spine. Thus,
the rank of an empty heap is zero, while the rank of a nonempty heap is one
more than the rank of its right child. For efficiency, we require every heap
to satisfy the **rank invariant**: the rank of a nonempty heap is stored at
the root of this heap.

**Question 1.** Define a function `rank : heap -> rank` such that `rank h` is
the rank of the heap `h` and `rank` has constant time complexity.

A heap must satisfy the **leftist invariant**: the rank of a left child is at
least as large as the rank of its right sibling. In other words, every
nonempty subheap `T (r, x, h1, h2)` must satisfy `rank h1 >= rank h2`.

In order to guarantee that every newly-built heap `T (_, _, _, _)` satisfies
the rank invariant and the leftist invariant, we define a smart constructor
`makeT`.

**Question 2.** Define a function `makeT : element -> heap -> heap -> heap`
such that the heap `makeT x h1 h2` has the element `x` at the root and has
`h1` and `h2` as its children. It is permitted to assume that the priority of
`x` is less than or equal to the priority of every element in `h1` and `h2`.
The heap `makeT x h1 h2` must satisfy all three invariants. Hint: you may
choose which of `h1` and `h2` should be the left child, and which should be
the right child.

Once `makeT` is available, there is never a need to use `T` directly to
construct a heap.

**Remark.** The leftist invariant implies that the length of a heap's right
spine is logarithmic in the size of this heap. Indeed, one can prove by
induction that the size of every heap `h` must be at least `2^(rank h) - 1`.
As an exercise, do this proof on paper; it is easy!

**Question 3.** Define a function `singleton : element -> heap` such that
`singleton x` is a heap whose multiset of elements is the singleton `{x}`.

## Merging two leftist heaps

Let us climb the highest mountain first, namely `union`.
Once this operation is implemented, the rest is easy.

**Question 4.** Define a function `union : heap -> heap -> heap`
such that `union h1 h2` is a heap whose multiset of elements is the
union of the multisets of elements of the heaps `h1` and `h2`.
The time complexity of `union` **must be linear** in `rank h1 + rank h2`.
Hint: the cases where `h1` or `h2` is empty are easy.
Hint: when `h1` and `h2` are both nonempty,
first ask yourself which element should be the root of the new heap;
then ask yourself what the two subheaps should be.

## Inserting an element

**Question 5.** Define a function `insert : element -> heap -> heap` such that
the heap `insert x h` contains the element `x` and the elements of the heap
`h`.

## Extracting a most urgent element

**Question 6.** Define a function `extract : heap -> (element * heap) option`
such that:

* `extract`, applied to an empty heap, returns `None`;
* `extract`, applied to a nonempty heap `h`, returns `Some (x, h')`, where
   `x` is an element of `h` of minimal priority and
   `h'` is the heap `h` deprived of `x`.
