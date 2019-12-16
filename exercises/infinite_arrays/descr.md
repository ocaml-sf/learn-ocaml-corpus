# Infinite Arrays

An OCaml array has a fixed, finite length. Indeed, when one allocates an array
via the function call `Array.make n x`, one must choose an integer value `n`,
which is the length of the newly created array. The cells of this array are
indexed by the integers between 0 (included) and `n` excluded. Every cell of
this array is initialized with the value `x`. An OCaml array is mutable: its
cells can be read and written via the operations `Array.get` and `Array.set`.
However, the length of an array cannot be changed.

In some situations, however, it is convenient to be able to work with
**infinite arrays**. Conceptually, infinite arrays work exactly like ordinary
arrays, except there is no need to specify their length at creation time:
their length is infinite. The function call `make x` creates a new infinite
array and initializes every cell in it with the value `x`. The cells of this
array are indexed by the natural integers, from zero to infinity. Arbitrary
cells are read and written via the operations `get` and `set`.

Thus, an implementation of infinite arrays must offer the following signature,
where `'a t` is the type of an infinite array whose elements have type `'a`:

```
  type 'a t
  val make: 'a -> 'a t
  val get: 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> unit
```

**Question.** Propose an implementation of infinite arrays.

*Hint.* Of course, one cannot allocate an OCaml array of infinite length. A
reasonable option, therefore, is to allocate an OCaml array of some finite
length `n`, and to adopt the convention that every cell in the infinite array
whose index is greater than `n-1` must contain the default value `x` that was
passed by the user as an argument to `make`. If and when the user wishes to
write a cell whose index `i` is greater than `n-1`, then a larger OCaml array
can be allocated. It is recommended that the size of the new OCaml array be at
least twice the size of the previous one, so as to amortize the cost of
initializing these arrays.
