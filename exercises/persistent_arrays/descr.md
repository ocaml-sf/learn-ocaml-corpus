# Persistent arrays

A **persistent** data structure is a data structure that appears to be
immutable, although it may internally involve mutable state.

A **persistent array** is a persistent data structure that represents a
sequence of elements. The length of a persistent array is fixed upon creation.
A persistent array supports efficient random access, via read and update
operations, `get` and `set`. `set` does not modify the persistent array that
it receives as an argument; instead, it returns a new persistent array.

This exercise does not require writing a lot of code: the complete solution is
under 20 lines of code. However, the code is slightly subtle, and requires
care.

This exercise is based on a short paper by
[Baker](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.3369) (1991).

## Data structure

The representation of persistent arrays in memory is subtle:
understanding it well requires some thought.

```
type 'a parray =
  'a representation ref

and 'a representation =
  | Array of { data: 'a array }
  | Apply of { base: 'a parray; i: int; value: 'a }
```

A persistent array `a` is a **reference** to an internal **representation**.

Because a persistent array is a reference, its representation can change over
time. However, as far as a user is concerned, this change must be invisible:
we must maintain the illusion that a persistent array is immutable. Ideally,
the type `'a parray` must be presented to a user as an abstract type.

Two internal representations exist:

* `Array { data }` means that the elements of this persistent array are stored
  in a primitive array `data`. This is a normal mutable OCaml array. Its
  content can change with time, provided (again) that this change remains
  invisible to an external user.

* `Apply { base; i; value }` means that the sequence of elements of this
  persistent array is the sequence of elements of the persistent array `base`,
  updated at index `i` with the value `value`.
  Thus, the sequence of elements of this persistent array is described
  indirectly as an update, a "diff", that should be applied to the elements
  of another persistent array.

It can be useful to think of a family of persistent arrays as a **graph**
where every vertex has zero or one outgoing edge.
A persistent array `a` of type `'a parray` forms a vertex.
If its current representation `!a` is `Array { data }`,
then it has no outgoing edge;
it just carries a pointer to a primitive array `data`.
If its current representation `!a` is `Apply { base; ... }`,
then it has an outgoing edge
towards the vertex `base`.

Because a persistent array `a` is a *reference* to a representation,
the representation of `a` can be modified by updating the reference `a`.
Thus, the shape of the graph can change over time.

## Creation and update

**Question 1.**
In one line,
define a function `make` of type `int -> 'a -> 'a parray`
such that `make n x` returns a persistent array
of size `n`
where every element is `x`.
This function should have time complexity O(n).

**Question 2.**
In one line,
define a function `set` of type `'a parray -> int -> 'a -> 'a parray`
such that `set base i value`
returns a persistent array
that is identical to `base`
except that its element at index `i` is `value`.
This function should have time complexity O(1).

## Read access

We begin with a naïve version of `get`.

**Question 3.**
Define a function `get` of type `'a parray -> int -> 'a`
such that `get a i`
returns the element found in the persistent array `a` at index `i`.
This function should not modify anything in memory.

With this naïve version of `get`,
when `get a i` is called,
the graph is traversed.
A path is followed,
starting at `a`
and following `Apply` edges,
up to a dead end,
that is, a vertex `z` whose representation `!z` is `Array { ... }`.

Therefore, the time complexity of `get` is O(m),
where `m` is the length of the path from `a` to `z`.
This is costly.

We would now like to define a smarter version of `get`,
where the work performed by a call to `get a i` allows
subsequent calls to `get a j` to be much faster.

The idea is to reverse the path from `a` to `z` in the graph.

Initially,
`!z` is `Array { data }`,
which means that a call to `get z j` takes time O(1),
because such a call just reads the `data` array.
In contrast, a call to `get a j` takes time O(m),
because such a call follows the path from `a` to `z`.

By reversing this path,
we wish to reach the opposite situation,
where `!a` is `Array { data }`
and there is a path of `Apply` edges from `z` to `a`.
As a result, a subsequent call to `get a j` takes time O(1).

Under a **locality** hypothesis, that is,
under the hypothesis that if a persistent array is read once,
then it is likely to be read again in the future,
this scheme is beneficial.

Naturally,
as we reverse the path,
we must update the `data` array
(which fortunately is a mutable OCaml array)
and annotate the newly-created `Apply` edges
with suitable `(i, value)` pairs
so that the end user
cannot observe these changes in the graph.

**Question 4.**
Define an auxiliary recursive function `revert` of type `'a parray -> 'a array`
such that `revert a` ensures that `!a` is `Array { data }`
and returns `data`. (Internally, `revert` follows the path
from `a` to `z`, where it finds the array `data`, then
reverses this path.)
Then, in one line, use `revert` to define
an optimized version of `get`.
