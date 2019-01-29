# A pretty-printer

In this problem, we build a slightly scaled-down version of
[pprint](https://github.com/fpottier/pprint/),
a library that helps display tree-structured data
as text
whose maximum width is fixed (to, say, 80 columns).

The user of the library describes the data as a **document**, a data type that
describes what should be displayed and several ways in which it could be
displayed. The library renders documents as text according to a fixed set of
rules.

Building and rendering a document are intended to
require linear time in the size of the document.

## Requirements

Before we can display a document, we must compute its horizontal space
requirement, that is, how much horizontal space this document would
require, if it were displayed on a single line.

Some documents, however, cannot be displayed on a single line; they require
several lines. We will consider that these documents have an infinite
horizontal space requirement.

Thus, a **requirement** is either `Infinite` or `Finite i`,
where `i` is a nonnegative integer:

```
type req =
  | Infinite
  | Finite of int
```

**Question 1.** Define a function `(++)` of type `req -> req -> req`
such that `req1 ++ req2` is the addition of the requirements `req1`
and `req2`.

**Question 2.** Define a function `(<==)` of type `req -> req -> bool`
such that `req1 <== req2` is true if and only the requirement `req1`
is less than or equal the requirement `req2`.

In the following, addition is used to compute the space requirement
of a composite document, whereas comparison is used to test whether
there remains enough space on the current line to display a document.

## Documents

The algebraic data type of **documents** is as follows:

```
type doc =
  | Empty
  | HardLine
  | Char of char
  | Cat of req * doc * doc
  | Nest of int * req * doc
  | Group of req * doc
  | IfFlat of doc * doc
```

## The meaning of a document

A document can be displayed in one of two modes, namely **flattening mode**
and **normal mode**. When displayed in flattening mode, a document fits on a
single line. When displayed in normal mode, a document can occupy one or more
lines.

The appearance of a document is also influenced by the current **indentation
level**, a nonnegative integer `n`. Every time a new line begins, `n` spaces
are printed.

With these two ideas in mind,
the meaning of each document constructor
can be described as follows:

* The document `Empty` is empty.

* The document `HardLine` imposes a line break,
  followed with `n` spaces,
  where `n` is the current indentation level.
  Therefore, this document cannot be displayed on a single line.
  The pretty-printer is designed in such a way
  that this document cannot be encountered when
  the printer is in flattening mode.

* The document `Char c` represents a single character, namely `c`.

* A composite document of the form `Cat (_, doc1, doc2)`
  represents the concatenation of the documents `doc1` and `doc2`.

* A document of the form `Nest (i, _, doc)` represents the document `doc`,
  inside which the indentation level has been changed from `n` to `n+i`.

* In flattening mode, a document of the form `Group (_, doc)` is treated like `doc`.
  In normal mode, the document `Group (_, doc)` denotes a choice between two
  ways of displaying the document `doc`. The first alternative is to enter
  flattening mode and display `doc` on a single line. (Any `Group` constructors
  inside `doc` are then ignored.) This alternative is permitted only if `doc`
  fits on the current line. The second alternative is to ignore this `Group`
  constructor and display `doc` in normal mode.

* The document `IfFlat (doc1, doc2)` is displayed as `doc1` in flattening mode
  and as `doc2` in normal mode.

## Well-formedness constraints

Documents are subject to the following well-formedness constraints:

* In `Char c`, the character `c` is never a newline character `'\n'`.

* In `IfFlat (doc1, doc2)`, the document `doc1` is never itself `IfFlat _`.
  That is, `IfFlat` cannot be nested in the left-hand side of `IfFlat`.

* In `Cat (req, doc1, doc2)`, `req` is the combined horizontal space
  requirement of `doc1` and `doc2`.

* In `Nest (i, req, doc)` and in `Group (req, doc)`,
  `req` is the horizontal space requirement of `doc`.

As indicated in the last two points above,
in a well-formed document,
some space requirements
have been computed ahead of time
and stored.
(This computation is the topic of Question 4.)

The last three points above are designed so as to allow the function
`requirement` (below) to have constant time complexity.

## Computing or fetching a space requirement

**Question 3.** Define a function `requirement` of type `doc -> req` such that
`requirement doc` is the horizontal space requirement of the document `doc`.
One may assume that `doc` is well-formed. This function must have **constant
time complexity**.

## Smart constructors

Allowing the end user to use the data constructors `Empty`, `Char`, etc.,
would be dangerous, as the user would then be able to build documents that do
not respect the well-formedness constraint described above. Furthermore,
exposing the concrete definition of the algebraic data type `doc` would limit
our ability to change this definition in the future.

For these reasons, we prefer to present `doc` as an abstract data type to the
end user. As a result, we must also expose a number of construction functions,
sometimes known as *smart constructors*. These functions are designed in such
a way that they always produce well-formed documents.

**Question 4.**
Define a smart constructor `char` of type `char -> doc`. For every character
`c`, including the newline character `'\n'`, `char c` must be a well-formed
document.
Define a smart constructor `(^^)` of type `doc -> doc -> doc`.
Define a smart constructor `nest` of type `int -> doc -> doc`.
Define a smart constructor `group` of type `doc -> doc`.
Define a smart constructor `ifflat` of type `doc -> doc -> doc`.

In the previous question, certain optional optimizations are permitted. For
instance, `(^^)` may recognize `empty` as a neutral element for concatenation.
`nest` may recognize that two consecutive `Nest` constructors can be fused.
`group` may recognize that two consecutive `Group` constructors can be fused.
`ifflat` must ensure that `IfFlat` never appears in the left-hand side of
`IfFlat` (this is necessary for its result to be well-formed), and may also
ensure that `IfFlat` never appears in the right-hand side of `IfFlat`.

## Rendering a document

The rendering engine, which transforms a document into a piece of text,
is implemented by a recursive function `render`. This function is then
wrapped in a simpler, non-recursive function, `pretty`.

The function `render` has a `state` parameter
whose type, also named `state`, is defined as follows:

```
type state =
  {
    width: int;
    mutable column: int;
    output: Buffer.t;
  }
```

This record holds the maximum text width (for instance, 80 columns),
the current column number, and
[an output buffer](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Buffer.html).

**Question 5.**
Define a recursive function `render` of type
`state -> int -> bool -> doc -> unit`.
This function takes four parameters,
namely the state `state`,
the current indentation level `n`,
the Boolean flag `flatten`,
which indicates whether flattening mode is active,
and a document `doc`.
The purpose of this function is to render the document `doc`
as text in the output buffer `state.output`.
In `render`, it is permitted to assume that
if `flatten` is `true`
then the document `doc` fits on the remainder of the current line.
(Dually, at every call to `render`, one must guarantee that
this property holds!)
Finally,
define a function `pretty` of type `int -> doc -> string`,
which allocates a new buffer, calls `render`, and returns
the final contents of the buffer.
