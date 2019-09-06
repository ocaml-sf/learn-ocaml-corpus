# The Union-Find data structure

In this exercise, we wish to develop an efficient implementation
of the Union-Find data structure.

## The Union-Find API

The signature of this data structure is as follows:

```
  type elem
  val make: unit -> elem
  val find: elem -> elem
  val eq: elem -> elem -> bool
  val union: elem -> elem -> unit
```

The data structure lets the user create an arbitrary number of **elements**
and organize them in groups, also known as *equivalence classes*. Whenever
desired, it is possible to merge two equivalence classes so that they become a
single equivalence class (a **union** operation) and to ask for the
representative element of an equivalence class (a **find** operation).

In other words, this data structure *maintains a partition under unions*.

The meaning of each element in the above signature can be briefly described as
follows:

* `elem` is the type of elements. As far as a user of the data structure
  is concerned, this type is abstract.

* `make()` creates and returns a new element. This element is initially
  isolated: that is, it forms an equivalence class of its own.

* `find x` returns the current representative element of the equivalence class
  of `x`. (In each equivalence class, an arbitrary representative element is
  distinguished. As long as no `union` operation affects this equivalence
  class, this representative element remains the same.)

* `eq x y` determines whether `x` and `y` are currently members of the same
  equivalence class.

* `union x y` merges the equivalence classes of `x` and `y`. (The
  representative element of the merged equivalence class is arbitrarily
  chosen.) If `x` and `y` are already members of the same equivalence class,
  this operation has no effect.

It may be worth emphasizing that this is a mutable data structure. The
operation `union` alters the data structure and cannot be undone.

## The Union-Find data structure

The Union-Find data structure is represented in memory in a very simple
manner. Elements are viewed as vertices in a directed acyclic graph. Every
vertex has either zero or one outgoing edge. The intuitive meaning of the
presence or absence of an edge is as follows:

* If an element `x` carries an outgoing edge towards another element `y`, then
  `x` and `y` are members of the same equivalence class.

  In essence, `x` says: "Don't look at me! Look at `y` instead. He is in my
  class, too."

* If an element `x` has no outgoing edge, then `x` is the representative
  element of its equivalence class.

  In essence, `x` says: "I am the class delegate."

The fact that the graph is acyclic means that every path is finite. By
starting at an element `x` and following outgoing edges as far as possible,
one necessarily finds the representative element `y` of `x`'s class.

Because the graph is acyclic, and because every element has at most one
outgoing edge, the graph is actually a forest: a collection of disjoint
trees. Every graph edge leads from one vertex to its parent in the forest.

For this reason, the Union-Find data structure
is also known as a **disjoint set forest**.

In OCaml, the data structure is described by the following type definitions:

```
  type elem =
    content ref

  and content =
  | Link of elem (* parent *)
  | Root of rank (* rank *)

  and rank =
    int
```

An element `x` is represented as a (mutable) heap-allocated object, a reference.

The *address* of this reference serves as the identity of this element.
Thus, two elements `x` and `y` are the same element if and only if the
physical equality `x == y` holds.

The *content* of this reference indicates whether this element carries an
outgoing edge. Thus,

* If `!x` is `Link y`,
  then there is an edge from the element `x` to the element `y`.

* If `!x` is `Root r`,
  then the element `x` has no outgoing edge.
  It is thus the representative element of its equivalence class.
  In that case,
  `x` carries an integer **rank** `r`.

In Question 1, ranks are not used; every rank can be zero.
In Question 2, ranks are used for balancing purposes.

The content of a reference can change with time: this allows graph edges to be
created, removed, or modified, if necessary.

The OCaml type definition shown above does not impose the property that the
graph is acyclic. It is up to each operation to maintain this invariant.

## Basic implementation

**Question 1.** Implement the operations `make`, `find`, `eq`, `union` in the
  simplest possible manner.

*Hint.* Before writing `union`, implement an auxiliary function `link` whose
type is the same as the type of `union`, that is, `elem -> elem -> unit`. The
purpose of the operation `link x y` is to merge the equivalence classes of `x`
and `y`, just like `union x y`. The difference between `union` and `link` is
that `link` is allowed to assume that `x` and `y` are the representative
elements of their equivalence classes, whereas `union` makes no assumption
whatsoever about `x` and `y`.

*Note.* In `link x y`, there is a choice between installing an edge from `x`
to `y` and installing an edge from `y` to `x`.

## Efficient implementation

The basic implementation that we have at this point presumably works fine, but
can be very slow. Indeed, the cost of a `find` operation is proportional to
the length of the path that it has to follow. In some scenarios, very long
paths could appear, so `find` could become very expensive (and `union`, which
uses `find`, would become very expensive as well). It is not difficult to see
that, in the worst case, the cost of *one* `find` or `union` operation can be
*linear* in the number of elements. This is very bad, as it implies that the
cost of a sequence of operations can be *quadratic* in the length of this
sequence.

To combat this problem, two optimizations come to mind: **balancing** and **path
compression**. Both aim to reduce the length of the paths that `find` has to
travel. Balancing avoids creating long paths in the first place, whereas path
compression shortens existing paths on the fly by installing shortcut edges.
Either optimization alone is effective; both optimizations combined are very
effective.

Path compression involves a simple change in the `find` operation. After
`find` follows a path `x`, `y`, ... that ends at a representative element `z`,
it is known that the representative element of the vertices `x`, `y`, ... is
`z`. Thus, direct edges from each of the vertices `x`, `y`, ... to `z` can be
installed in place of the existing edges. Thus, a possibly long path is
replaced with a collection of paths of length 1.

Balancing involves simple changes in the `link` operation. As noted earlier,
in `link x y`, there is a choice between installing an edge from `x` to `y`
and installing an edge from `y` to `x`. By adopting an appropriate strategy,
it is possible to ensure that every tree is balanced and (therefore) that
every path has at most logarithmic length. **Linking-by-rank**, one of several
possible balancing strategies, works as follows. Every root element carries an
integer rank. In `link x y`, the following rule is used:

* If `x` and `y` have distinct ranks,
  install an edge from the one that has smaller rank
  to the one that has greater rank.

* If `x` and `y` have the same rank,
  install an edge in an arbitrary direction,
  and increment the rank of the new root.

It is not difficult to see that, in the absence of path compression, the rank
of a root element `x` is exactly the maximum length of the paths that lead to
`x`. In other words, it is the height of the tree whose root is `x`.

**Question 2.** Implement path compression and linking-by-rank.

*Hint.* These two optimizations are independent of one another. Implement one
of them and test your code: at this point, it should still pass Question 1,
but will not pass Question 2 yet. Then, implement the second optimization and
test your code again: it should pass both Question 1 and Question 2.

*Note.* The automatic grading code assesses the complexity of your code by
executing scenarios that involve thousands of elements. It counts the number
of read operations on references `!x` performed by your code and by a
reference implementation. If the ratio between the two seems too high, your
code is deemed inefficient.

## Bibliographic notes

Disjoint set forests were invented by
[Galler and Fischer](http://doi.acm.org/10.1145/364099.364331) in 1964.
The idea of path compression is attributed to McIlroy and Morris.
Linking-by-rank has been studied by
[Tarjan](http://www.csd.uwo.ca/~eschost/Teaching/07-08/CS445a/p215-tarjan.pdf)
(1975).

It is easy to see that linking-by-rank alone guarantees that `find` has
logarithmic worst-case complexity.

Path compression alone guarantees that `find` has logarithmic worst-case
complexity (in an amortized sense). This was proved by
[Tarjan and van Leeuwen](http://dx.doi.org/10.1145/62.2160) in 1984. A
different proof is given by
[Seidel and Sharir](http://dx.doi.org/10.1137/S0097539703439088) (2005).

Together, path compression and linking-by-rank guarantee excellent efficiency.
When they are combined, the amortized complexity of `find` is alpha(n), where
alpha is a very, very, very slow-growing function. In other words, `find`
has almost-constant time complexity. The complexity of `union` is the same.
This was first proved by Tarjan in 1975.
A more recent treatment is given by
[Alstrup et al.](http://doi.acm.org/10.1145/2636922) (2014).

The Union-Find data structure serves as a key basic block in many algorithms,
including [unification](https://en.wikipedia.org/wiki/Unification_(computer_science)),
a key component of many automated and interactive theorem provers.
