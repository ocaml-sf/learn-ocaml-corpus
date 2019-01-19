# Random access lists

This problem is based on Chapter 10.1.2 of Chris Okasaki's book,
[Purely Functional Data Structures](https://www.cambridge.org/fr/academic/subjects/computer-science/programming-languages-and-applied-logic/purely-functional-data-structures?format=PB&isbn=9780521663502).

## Data structure

A **random access list** is a data structure that represents a **sequence** of
elements. It is so named because it allows fast random access, that is, fast
access to an element at an arbitrary index.

A sequence either is empty, or has an even number of elements, or has an odd
number of elements. These three cases are reflected in the definition of the
type `'a seq`:

```
type 'a seq =
| Nil
| Zero of     ('a * 'a) seq
| One of 'a * ('a * 'a) seq
```

An empty sequence is represented by the data constructor `Nil`.

A nonempty sequence of even length is represented as an application of the
data constructor `Zero`. Because the sequence has even length `n`, it can be
organized as *a sequence of pairs* of length `n/2`, whose type is `('a * 'a) seq`.

A nonempty sequence of odd length is represented as an application of the
data constructor `One`. The first element of the sequence, whose type is `'a`,
s stored at the root of the random access list. The remainder of the sequence,
which has even length, is again organized as a sequence of pairs,
whose type is `('a * 'a) seq`.

As a result of these decisions, a sequence of `n` elements is represented in
memory as a list of `Zero` and `One` digits, which corresponds to the
representation of the number `n` in binary notation. (The least significant
digit is at the head of this list.) The length of this list is obviously
logarithmic in `n`. Furthermore, each element of this list is a pair of pairs
of pairs (...) whose depth is also logarithmic in `n`. This is why the random
access operations `get` and `update` have logarithmic time complexity.

Since `Zero` and `One` are used only when the sequence is nonempty,
`Zero` is never followed with `Nil`.

## Examples

**Question 1.**
Define a random access list `empty` of type `'a seq` which represents
the empty sequence.

The number *two* is written `10` in ordinary binary notation (most significant
digit first), hence `01` in reverse (least significant digit first).
Therefore, a sequence of *two* elements is represented by a random access list
whose shape is `Zero (One (_, Nil))`.

**Question 2.**
Define a random access list `test24` of type `int seq` which represents
the sequence of integers `2, 4`.

**Question 3.**
Define a random access list `digits` of type `int seq` which represents
the sequence of integers `0, 1, 2, 3, 4, 5, 6, 7, 8, 9`.

## Length

We now wish to define a function `length` such that `length xs` is the number
of elements of the random access list `xs`. In other words, `length xs` is the
length of the sequence represented by `xs`.

The type of `length` is `'a seq -> int`. This function is **polymorphic**: it
works for every element type `'a`. This can be emphasized by using an explicit
**universal quantification**: the type of `length` can also be written as `'a
. 'a seq -> int`. Indeed, **for every** `'a`, applying `length` to a value of
type `'a seq` produces an integer result.

When `length` is applied to a sequence of type `'a seq`, it may need to apply
itself, recursively, to a (shorter) sequence of type `('a * 'a) seq`. This
phenomenon is known as **polymorphic recursion**. Fortunately, because
`length` is polymorphic, this is permitted, provided the recursive function is
explicitly annotated with a polymorphic type: this helps the OCaml
type-checker understand what is going on.

**Question 4.**
Define the function `length`.

## Insertion and extraction at the front

**Question 5.**
Define a function `cons` of type `'a . 'a -> 'a seq -> 'a seq`
such that the sequence `cons x xs`
begins with the element `x`
and continues with the elements of the sequence `xs`.

**Question 6.**
Define a function `uncons` of type `'a . 'a seq -> ('a * 'a seq) option`
such that `uncons empty` is `None`
and `uncons (cons x xs)` is `Some (x, xs)`.

## Random access

In the following,
a valid index into a random access list `xs`
is an integer `i` that lies between `0` included and `length xs` excluded.

We begin with read access:

**Question 7.**
Define a function `get` of type `'a . int -> 'a seq -> 'a`
such that `get i xs` is the element at index `i` in the sequence `xs`.
One can assume that `i` is a valid index into `xs`.

We continue with write access.

We wish to define a function
`update` of type `'a . int -> 'a -> 'a seq -> 'a seq`
whose behavior can be described as follows:

* The length of `update i x xs` is the length of `xs`.

* The `i`-th element of `update i x xs` is `x`.

* For every index `j` other than `i`,
  the `j`-th element of `update i x xs` is the `j`-th element of `xs`.

**Question 8.**
Try giving a direct recursive definition of `update`.
You will find that this approach does not work.
Yet, this is not a waste of time; this attempt
serves as a preparation for the next question.

To work around this problem,
we first define a more general function
`fupdate` of type `'a . int -> ('a -> 'a) -> 'a seq -> 'a seq`
whose behavior can be described as follows:

* The length of `fupdate i f xs` is the length of `xs`.

* The `i`-th element of `fupdate i f xs` is `f (get i xs)`.

* For every index `j` other than `i`,
  the `j`-th element of `fupdate i f xs` is the `j`-th element of `xs`.

In other words, the job of `fupdate` is to apply the function `f`
to the `i`-th element of the sequence `xs`,
producing a new sequence.

**Question 9.**
Building on the failed attempt of the previous question,
define the function `fupdate`.
Then, using `fupdate`, define `update`.

## Application

As an application of random access lists,
we wish to write an **evaluator**
of arithmetic expressions.
Our arithmetic expressions involve constants,
binary operators,
and variables,
which are locally defined by a `let` construct.
For instance, `let x = 24 + 18 in x * x` is a
valid arithmetic expression, expressed in concrete syntax.

The algebraic data type `expr` of arithmetic expressions is defined as follows:

```
type constant =
  int

type var =
  int (* a de Bruijn index *)

type op =
  int -> int -> int

type expr =
  | EConstant of constant
  | EBinOp of expr * op * expr
  | EVar of var
  | ELet of expr * expr
```

The expression `let x = 24 + 18 in x * x`
is expressed as follows in this abstract syntax:

```
  Let (
    EBinOp (
      EConstant 24,
      ( + ),
      EConstant 18
    ),
    EBinOp (
      EVar 0,
      ( * ),
      EVar 0
    )
  )
```

The index carried by `EVar` is a **de Bruijn index**. `0` denotes the variable
that was defined by the nearest enclosing `ELet` construct; `1` denotes the
variable that was defined by the second-nearest enclosing `ELet` construct;
and so on.

We now wish to write an evaluator, that is, a function `eval` that accepts an
arithmetic expression `e` and computes its value.

An evaluator also takes an **environment** `env` as a parameter.
This environment maps each of the variables that appear free in `e`
to the value of this variable.

For efficiency,
an environment is represented in memory as a random access list:

```
type env =
  constant seq
```

The value at index `0` in the environment represents the value of the
variable `0`, and so on.

**Question 10.**
Define a function `eval` of type `env -> expr -> constant`,
such that `eval env e`
is the value of the expression `e` under the environment `env`.

In particular,
if the expression `e` has no free variables,
then `eval empty e` is the value of `e`.
