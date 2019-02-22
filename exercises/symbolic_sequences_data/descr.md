# Symbolic Sequences as Objects

In this exercise, we implement an immutable data structure
which represents a **sequence** of elements.

Unlike an ordinary *container* data structure, such as a list,
this data structure is **symbolic**: it is a *description* of a
sequence. The elements of the sequence do not actually exist in
memory. It is possible to build compact descriptions of huge
sequences.

This data structure is designed so that construction is cheap:
every **constructor** function has constant time complexity.

There are three **destructor** functions, also known as **accessors**. The
function `length` returns the **length** of a sequence; it also takes constant
time. The **random access** function `get` returns the `i`-th element of a
sequence. The **iteration** function `foreach` provides access to each element
of the sequence in turn. The complexity of `get` and `foreach` is a little
more difficult to describe; let us say that, if a sequence is built without
using `map`, then the complexity of `get` is linear in the depth of the
sequence description, and the complexity of `foreach` is linear in the length
of the sequence plus the size of the sequence description.

**In this exercise, we implement symbolic sequences as data**, that is,
as a (generalized) algebraic data type. In a companion
exercise, entitled "Symbolic Sequences as Objects", we implement them as
records of functions. Although these two approaches give
rise to two different ways of organizing the code, the underlying algorithm is
the same. In fact, the GADT-based code is the image of the object-based code
through a mechanical transformation known as
[defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization).

In another exercise, entitled "Enumerating Trees",
we *use* symbolic sequences.

## Interface

Before explaining how symbolic sequences are implemented,
let us first describe their interface,
that is,
the operations that they support.

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

**Note.**
Because sequences can be extremely long,
OCaml's built-in type `int`, which is 63 bits wide at best,
can be insufficient to represent the length of a sequence.
In order to avoid overflows, we *should* use a type of unbounded integers,
such as the type `Z.t` provided by the library `zarith`.
However, for the sake of simplicity,
in this exercise, we stick with the type `int`.

## Implementation

In this exercise, we represent a sequence in memory as a value of the
following algebraic data type:

```
type _ seq =
| Empty    : 'a seq
| Singleton: 'a -> 'a seq
| Sum      : int * 'a seq * 'a seq -> 'a seq
| Product  : int * 'a seq * 'b seq -> ('a * 'b) seq
| Map      : int * ('a -> 'b) * 'a seq -> 'b seq
```

This is a *generalized* algebraic data type.
In an ordinary algebraic data type,
every data constructor has result type `'a seq`.
Here, the constructor `Product` violates this rule:
its result type is `('a * 'b) seq`.

The data constructors `Sum`, `Product` and `Map` carry
the length of the sequence, which has type `int`,
as their first argument.
This allows a constant-time implementation of the function `length`.

## Assignment

**Question 1.** Implement `length`.

**Question 2.** Implement the five constructors presented above,
in one line each.

**Question 3.** Implement `get`.

**Question 4.** Implement `foreach`.

*Note.* The messages produced by the automatic grader for
Question 4 may refer to the function `elements`. This
function takes a `foreach` function as an argument and
builds a list of the elements produced by `foreach`.
It is defined as follows:

```
let elements (foreach : ('a -> unit) -> unit) : 'a list =
  let xs = ref [] in
  foreach (fun x -> xs := x :: !xs);
  List.rev !xs
```
