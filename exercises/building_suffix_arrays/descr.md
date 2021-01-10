# Building Suffix Arrays

A [suffix array](https://en.wikipedia.org/wiki/Suffix_array)
is a compact data structure that serves as a
basis for efficient searching.

## What is a Suffix Array?

A *string* is a finite sequence of characters. For example, here is
a string of 10 characters:

```
  mississipi
```

A *prefix* of this string is obtained by keeping only the first `k` characters
of the string, for some integer `k`. A *suffix* of this string is obtained by
keeping only its last `k` characters. Thus, the string `mississipi` has 10
nonempty suffixes:

```
  mississipi
   ississipi
    ssissipi
     sissipi
      issipi
       ssipi
        sipi
         ipi
          pi
           i
```

A suffix array is the result of sorting these suffixes in lexicographic order:

```
  i
  ipi
  issipi
  ississipi
  mississipi
  pi
  sipi
  sissipi
  ssipi
  ssissipi
```

Because a sorted list supports searching in logarithmic time, this is a very
useful data structure.

If a string `s` has `n` characters, then it has `n` nonempty suffixes, each of
which has length at most `n`, so the size of a suffix array would be
proportional to `n^2`, if one used a naïve representation.

However, it is easy to imagine how one can do much better. Provided the string
`s` has been loaded into memory and remains available, a suffix of `s` can be
represented by its start index `k`. Thus, one suffix is represented by just
one integer, and a suffix array can be represented as an array of `n`
integers.

The suffixes of the string `mississipi`, in numeric order, are the integers
in the semi-open interval `[0, 10)`, that is, the integers comprised between
0 (included) and 10 (excluded):

```
  mississipi  0
   ississipi  1
    ssissipi  2
     sissipi  3
      issipi  4
       ssipi  5
        sipi  6
         ipi  7
          pi  8
           i  9
```

Sorting this list lexicographically produces the following result:

```
  i           9
  ipi         7
  issipi      4
  ississipi   1
  mississipi  0
  pi          8
  sipi        6
  sissipi     3
  ssipi       5
  ssissipi    2
```

Thus, the suffix array associated with the string `mississipi` is
this array of integers:

```
              9
              7
              4
              1
              0
              8
              6
              3
              5
              2
```

that is, in OCaml notation, the array `[|9; 7; 4; 1; 0; 8; 6; 3; 5; 2|]`.

This array is a permutation of the semi-open interval `[0, 10)`: that is,
each integer index comprised between `0` (included) and `10` (excluded)
appears exactly once in this array.

## Preliminaries

In this exercise, we manipulate many arrays. Thus, we wish to refer to
the most common operations on arrays via short names. Our prelude makes
the following definitions:

```
  let copy = Array.copy
  let init = Array.init
  let iter = Array.iter
  let length = Array.length
  let make = Array.make
  let map = Array.map
```

These functions are provided by the module
[Array](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html)
of the OCaml standard library.
We also use several of the functions provided by the modules
[String](https://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html)
and
[List](https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html),
but do not introduce short names for these functions.

As announced earlier, a suffix of a string `s` is represented as an index into
the string `s`, its start index. Thus, we make the following type definition:

```
  type suffix = int
```

While programming and debugging, it is convenient to have a function that
converts a value of type `suffix` to an actual suffix, that is, a string.
By mapping this function over a suffix array, we obtain a function that
converts a suffix array to a human-readable form. Thus, our prelude makes
the following definitions:

```
  let suffix (s : string) (i : suffix) : string =
    let n = String.length s in
    String.sub s i (n-i)

  let suffixes (s : string) (a : suffix array) : string array =
    map (suffix s) a
```

As an example, try evaluating the OCaml expression
`suffixes "mississipi" [|9; 7; 4; 1; 0; 8; 6; 3; 5; 2|]`
and check that you get the result that you expect.

Because the function `suffixes` turns a suffix array of size `n` into a string
array whose size is proportional to `n^2`, it should never be used as part of
an efficient algorithm. It is meant to be used only while programming and
debugging.

Our prelude also defines a function that extracts a prefix out of a string:

```
  let prefix (h : int) (s : string) : string =
    let n = String.length s in
    if n <= h then s else String.sub s 0 h
```

## Recognizing a Suffix Array

As a warm-up, and before attempting to build a suffix array, we first write
code that checks whether an array `a` is the suffix array of the string `s`.

**Question 1.** Write a function `is_sorted`
of type `('a -> 'a -> bool) -> 'a array -> bool`
such that `is_sorted (<=) a`
determines whether the array `a` is sorted
with respect to the preorder `(<=)`.

**Question 2.** Write a function `is_permutation`
of type `int array -> bool`
such that `is_permutation p`
determines whether the array `p` represents a valid
permutation of the semi-open interval `[0,n)`,
where `n` is the length of the array `p`.

**Question 3.** Write a function `leq_suffix_suffix`
of type `string -> suffix -> suffix -> bool`
such that `leq_suffix_suffix s i j` is equivalent to
`suffix s i <= suffix s j`.
That is, `leq_suffix_suffix i j` determines whether
the suffixes `i` and `j` are lexicographically ordered.

*Note.* It is possible to answer Question 3 in one line by defining
`let leq_suffix_suffix s i j = (suffix s i <= suffix s j)`. However,
that is a naïve and inefficient solution. Because the function calls
`suffix s i` and `suffix s j` allocate new strings in memory, their
time complexity is *always* `Omega(n - i + n - j)`, where
`n` is the size of the string `s`. Instead, it is possible to
implement `leq_suffix_suffix` in such a way that it does not
allocate any memory and terminates in time proportional to `k`,
where `k` is the length of the longest common prefix of the strings
`suffix s i` and `suffix s j`. Although the worst-case time complexity
of this solution remains `O(n)`, it is much faster in practice than
the naïve solution.

**Question 4.** Write a function `is_suffix_array`
of type `string -> suffix array -> bool`
such that `is_suffix_array s a` determines
whether `a` is the suffix array of the string `s`.

## Building a Suffix Array, Naïvely

The function `leq_suffix_suffix` compares two suffixes in time `O(n)`.
A sorting algorithm, such as OCaml's
[`Array.sort`](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html),
requires `O(n.log n)` comparisons.
Thus, it is easy to build a suffix array in time `O(n^2.log n)`.

**Question 5.** Write a function `naive_suffix_sort`
of type `string -> suffix array` such that
`naive_suffix_sort s` is the suffix array of the string `s`.

*Note.* The function `leq_suffix_suffix` returns a Boolean result, whereas the
function `Array.sort` expects the comparison function to return a three-way
result (that is, one of "smaller", "equal", or "greater"), encoded as an
integer value. So, you will need to work a little bit to connect these
functions.

*Note.* For every string `s`, the expression `is_suffix_array s (suffix_sort
s)` should evaluate to `true`. You can use this property to test your code
before running the automatic grading system.

## Building a Suffix Array, Efficiently

In the remainder of this exercise, we would like to implement an *efficient*
algorithm for building a suffix array.

The algorithm that we present was proposed by Udi Manber and Gene Myers in the
paper [Suffix arrays: a new method for on-line string
searches](https://epubs.siam.org/doi/10.1137/0222058)
(1990). The complexity of this algorithm is *O(n.log n)*,
where *n* is the size of the string of interest.

As we will see, Manber and Myers' algorithm itself is not exactly
straightforward: understanding it requires some thought and dedication.

The algorithm works in a number of stages, which we refer to as *stage 0*,
*stage 1*, *stage 2*, *stage 4*, *stage 8*, and so on.

* At the beginning of stage 0, we construct an (unsorted) array *a* of all
  nonempty suffixes. During stage 0, we sort these suffixes according to
  the *first character* of each suffix only.

* At the beginning of stage *h*, where *h* is a power of two (1, 2, 4, 8,
  etc.), we assume that the array *a* is sorted according to the *h* first
  characters of each suffix. During stage *h*, we sort this array in a
  stronger way, so that, at the end of this stage, it is sorted according
  to the *2h* first characters of each suffix. This allows us to move on
  to the next stage.

Once *h* exceeds *n*, the array *a* is sorted, and is therefore a suffix
array.

Manber and Myers' insight is that it is possible to execute each stage in time
*O(n)*. Furthermore, because *h* doubles at each stage, the number of
necessary stages is *O(log n)*. Therefore, the time complexity of the
algorithm is *O(n log n)*.

## Stage 0: Sorting the Suffixes according to their First Character

The aim of stage 0 is to sort an array of the nonempty suffixes
according to their first characters. This can be
done in linear time using a [pigeonhole sort](https://en.wikipedia.org/wiki/Pigeonhole_sort).

**Question 6.** Define a function `pigeonhole_sort`
of type `int -> ('a -> int) -> 'a array -> unit`
such that `pigeonhole_sort m key a`
sorts the array `a` by key.
The function `key` maps an element of type `'a`
to an integer in the semi-open interval `[0, m)`.
The time complexity of the algorithm must be `O(m + n)`,
where `n` is the length of the array `a`.
The sorting algorithm must be stable: that is, if two
values have the same key, then they must not be exchanged.

*Hint.* Build an array `bucket` of size `m` such that
`bucket.(k)` holds a list of all values whose key is `k`.
Then, write the data back to the array `a`, in an appropriate order.

One can use this sorting algorithm to sort an array of strings by their
first character. Assuming that one works with the ASCII character set,
a character is represented as a 7-bit integer code, that is, an
integer value in the interval `[0, 128)`. The function
`Char.code` maps a character to its integer code. Thus,
to sort an array `a` of strings, one can write

```
  pigeonhole_sort 128 (fun s -> Char.code s.[0]) s
```

By applying this procedure to the array of suffixes of the
string `"mississipi"`,

```
[|
  "mississipi";
   "ississipi";
    "ssissipi";
     "sissipi";
      "issipi";
       "ssipi";
        "sipi";
         "ipi";
          "pi";
           "i";
|]
```

we obtain the following sorted array:

```
[|
  "ississipi";
  "issipi";
  "ipi";
  "i";
  "mississipi";
  "pi";
  "ssissipi";
  "sissipi";
  "ssipi";
  "sipi";
|]
```

This array is divided into *buckets*, or ranges of elements that begin
with the same letter. There is a bucket of four strings that begin with
the letter `'i'`, a bucket of one string that begins with the letter `'m'`,
a bucket of one string that begins with the letter `'p'`, and a bucket of
four strings that begin with the letter `'s'`.

In the following, it is necessary for us to know where each bucket begins.
This information can easily be computed in linear time. This is the topic
of the next question.

**Question 7.** Define a function `beginning_of_bucket` of type
`('a -> int) -> 'a array -> bool array`, such that,
under the assumption that the array `a` is sorted by `key` already,
the function call `beginning_of_bucket key a`
produces an array `b` such that `b.(i)` is `true`
if and only if the index `i` marks the beginning of a new bucket,
that is, if and only if `i` is zero or the element `a.(i)` does not
have the same key as its predecessor.

*Example.* If `s` denotes the sorted array of the suffixes of the string
`"mississipi"`, as shown above, then the expression:

```
  beginning_of_bucket (fun s -> Char.code s.[0]) s
```

should produce the following Boolean array:

```
  [|true; false; false; false; true; true; true; false; false; false|]
```

## Stages 1, 2, 4, 8...: Jumping from *h* to *2h*

At the beginning of stage *h*, where *h* is at least 1, we have an array *a*
containing all of the nonempty suffixes of the string *s*.
Assuming *s* is a string of length *n*, a nonempty
suffix of *s* is represented as an integer in the semi-open interval
*[0, n)*. Thus, the array *a*, whose type is `suffix array`, is a
permutation of *[0, n)*.

At the beginning of stage *h*, we assume that the array *a* is *h*-sorted,
that is, sorted according to the *h* first characters of each suffix. It is
useful to think of the array *a* as partitioned into a number of
*h*-*buckets*, where all of the suffixes inside each bucket are *h*-equal,
that is, they agree on their *h* first characters.

We assume that a Boolean array *b*, which contains beginning-of-bucket
marks, is also given.

### Aim

The aim of this stage is to reach a situation where the array *a* is
*2h*-sorted. This is a more precise preorder: indeed, two suffixes that are
*2h*-equal must also be *h*-equal. This means that we wish to *refine* the
current partition. In other words, we wish to *divide* each *h*-bucket into
one or more *2h*-buckets.

To do so, we must move some suffixes to a different place in the array *a*.
These moves take place only *within* an *h*-bucket, never from an
*h*-bucket to another.
We must also update the array *b* so as to mark the beginning
of each *2h*-bucket.

It is worth noting that the suffixes in the interval *[n-h+1, n)* will not
move during this stage. Indeed, these suffixes have fewer than *h* characters,
so, as far as they are concerned, *h*-order and *2h*-order are the same thing.
Because *a* is *h*-sorted, each of these suffixes has already reached its
final position in the array, and inhabits a singleton *h*-bucket.

### First Insight

How can we implement this stage efficiently, in time *O(n)*?

Manber and Myers' first insight is that the necessary comparisons *have
already been performed* during previous stages! Indeed, suppose that two
suffixes *d1* and *d2* lie in the same *h*-bucket. In order to determine in
what order they should appear at the end of this stage, we need to
*2h*-compare them, that is, to compare their *2h* first characters. However,
we already know that their *h* first characters coincide. So, we need to
compare the *next* run of *h* characters, between offsets *h* (included) and
*2h* (excluded). In other words, we need to *h*-compare the suffixes *d1+h*
and *d2+h*. (Let us assume in this informal explanation that *d1+h* and *d2+h*
are both less than *n*, so they are well-defined suffixes.)
This is something that we have already done during previous stages!
Since the array *a* is *h*-sorted, all we need to do is find out which of the
suffixes *d1+h* and *d2+h* appears first in the array *a*. With appropriate
preprocessing (see following remark), this can be done in constant time.

*Remark.* The permutation *a* can be inverted in time *O(n)*. Its inverse is
an array that maps every suffix *d* to its position in the array *a*. Once
this inverse permutation has been computed, we can find out in constant time
where in the array *a* a suffix *d* is stored.

To sum up, Manber and Myers' first insight is that two suffixes can be
*2h*-compared in constant time.

Thus, a naïve idea would be to implement stage *h* simply by sorting the array
*a* using an off-the-shelf sorting algorithm, such as Heapsort or Mergesort.
The total complexity of this stage would then be *O(n log n)*, which is not
too bad.

### Second Insight and Pseudocode

Yet, one can do better! Manber and Myers' second insight is that it is
possible to move every suffix to a correct position in time *O(n)*.

To explain this, imagine that we keep the arrays *a* and *b* in their original
state, and we create two new arrays *a'* and *b'* that describe the new state.
(In practice, one may be able to re-use the existing storage instead of
allocating new storage; this is an implementation detail.)

From the array *a*, we will read all suffixes,
from left to right, in *h*-order.
Into the array *a'*, which is initially a copy of *a*,
we will write all suffixes in the range *[0,n-h]*,
in *2h*-order, at appropriate positions.
Of course, we cannot write them all in a single
left-to-right pass.
Instead, we rely on the existing boundaries
between *h*-buckets, which we keep.
We think of each *h*-bucket as initially empty,
so we view the array *a'* as a series of initially empty *h*-buckets.
Inside each such *h*-bucket, we write from left to right.
(This requires us to keep track, inside each *h*-bucket, of the
boundary between the slots that have been written already and
the slots that remain available.)

At a high level of abstraction, the algorithm can be described as follows:

* By reading the arrays *a* and *b*,
  build a list of all *h*-buckets,
  ordered from left to right.

* In front of this list,
  insert an extra bucket that contains just the empty suffix,
  represented by the integer *n*.
  (The empty suffix is smaller, in *h*-order,
  than any nonempty suffix.)

* The list of buckets thus obtained forms a partition of the closed interval *[0, n]*.

* For each bucket *bucket* in this list, from left to right, do the following:

  * For each suffix *d* in *bucket*, from left to right, do the following:

    * Let *d'* stand for *d-h*.
      Assuming *d'* is a well-defined suffix (that is, assuming it is nonnegative),
      find out in which *h*-bucket *bucket'* of the array *a* this suffix lies.
      Then, write *d'* into the first available slot of the bucket *bucket'*
      in the array *a'*.
      If this is the first element of *bucket* that has been written to
      *bucket'*, then mark this slot as the beginning of a *2h*-bucket
      in the array *b'*.

One can see that every suffix *d'* in the closed interval *[0,n-h]* is considered
exactly once and is therefore written exactly once to the array *a'*. The suffixes
in the interval *[n-h+1,n)* are not considered and do not move: their position in
the array *a'* is the same as in the array *a*.

### Now, Up to You

Congratulations for reading this far! It is now up to you to put everything
together:

**Question 8.** Complete the function `stage`,
whose arguments are the string *s*, the positive integer *h*,
and the arrays *a* and *b*.
The array *a* is assumed to be *h*-sorted.
The array *b* is assumed to contain beginning-of-*h*-bucket marks.
The function `stage` should first test whether the task is complete,
that is, whether the array *a* is in fact sorted.
If so, it should return *a*.
Otherwise, it should perform stage *h*,
thus ensuring that the array *a* is *2h*-sorted
and that the array *b* contains beginning-of-*2h*-bucket marks.
It can then invoke itself recursively.
Once this is done, write a function `suffix_sort`
of type `string -> suffix array` such that
`suffix_sort s` is the suffix array of the string `s`.
This function should first perform stage 0,
then invoke `stage` to perform stages 1, 2, 4, etc.

*Hint.* To help you take care of the details, a number of
auxiliary functions are provided. In short, `invert` inverts a permutation;
`nearest_set_bit_left` searches for the beginning of a bucket;
`population` counts the number of buckets;
`buckets` builds a list of buckets
that corresponds to steps 1-3 of the algorithm above;
and the functions in the module `Allocator`
help deal with the concept of the *next available slot*
in each *h*-bucket of the array *a'*.

*Hint.* It is important to keep the time complexity of each stage *O(n)*.
If the automated grading system seems to run out of time, it may be the case that
your algorithm is functionally correct but does not have the expected
time complexity.

Good luck,
and (anticipating on your success)
congratulations for implementing this nontrivial algorithm!
