# Huffman Coding

[Huffman coding](https://en.wikipedia.org/wiki/Huffman_encoding)
is a technique for encoding a sequence of characters (the
*input data*) as a sequence of bits (the *encoded data*).

We refer to the characters that appear in the input data as *input
characters*. The set of all input characters is the *input alphabet*.

Huffman coding is based on a **fixed dictionary**: that is, each character in
the input sequence is encoded as a fixed sequence of bits. This dictionary is
constructed in such a way as to minimize the length of the encoded data, in
the hope of achieving some compression.

In order to permit unambiguous decoding, two distinct input characters must be
encoded as two **unrelated** sequences of bits, where two sequences are
unrelated if neither is a prefix of the other. Indeed, imagine what would
happen if, for instance, the character `'a'` was encoded as `"0"` and the
character `'b'` was encoded as `"00"`. Then, the encoded data `"00"` would be
ambiguous: it would represent both the character sequence `"aa"` and the
character sequence `"b"`. This problem arises because the bit sequences `"0"`
and `"00"` are related: the former is a prefix of the latter.

How can one guarantee, by construction, that the encodings of any two distinct
input characters are unrelated? The best answer to this question is to
represent a dictionary as a **tree** where every leaf carries an input
character and every internal node carries two subtrees:

```
  type tree =
  | Leaf of char
  | Node of tree * tree
```

A path from the root to a leaf can be encoded as a bit string, where the bit
`0` means "go down towards the left child" and the bit `1` means "go down
towards the right child". Thus, for instance, in the tree `Node (Leaf 'a',
Node (Leaf 'b', Leaf 'c'))`, the leaf labeled `'a'` appears at the end of the
path `"0"`, the leaf labeled `'b'` appears at the end of the path `"10"`, and
the leaf labeled `'c'` appears at the end of the path `"11"`. This represents
a dictionary where `'a'` is encoded as `"0"`, `'b'` is encoded as `"10"`, and
`'c'` is encoded as `"11"`.

The problematic dictionary where the character `'a'` is encoded as `"0"` and
the character `'b'` iss encoded as `"00"` cannot be represented by such a
tree. (Think about it. The left child of the root node would have to be both a
leaf and an internal node.)

A valid tree is expected to satisfy the following properties:

* Every character that appears at a leaf is indeed an input character.

* Every input character appears at some leaf.

* No two leaves carry the same character.

* This tree is optimal in the following sense:

  + Using this tree, the input data is encoded as
    a sequence of bits of length `k`;

  + No other tree allows the input data to be encoded as
    a sequence of bits of length less than `k`.

Please pause for a moment and make sure that you understand why each of the
four above properties is desirable.

In this exercise, we wish to perform the following tasks:

* Out of a piece of input data,
  build a dictionary,
  that is,
  an optimal Huffman tree.

* Out of a dictionary
  and a piece of encoded data,
  recover
  the original input data.

* Find a way of encoding a dictionary itself
  as a sequence of bits,
  and a way of reading it back.

* Combine these pieces to define stand-alone
  compression and decompression
  functions.

## Some Definitions

We refer to the input data, which we wish to encode, as *text*.
It is a sequence of characters. We represent it in OCaml as a
value of type `string`.

```
  type text =
    string
```

We represent encoded data, or *data* for short, as a string where every
character is either the character `'0'` or the character `'1'`. (We choose
this representation for the sake of simplicity. Because each character in an
OCaml string occupies 8 bits, this is a naïve representation, which occupies 8
times more space than really necessary. In a real-world implementation, one
would arrange for binary data to be represented as a sequence of bits.)

```
  type data =
    string
```

In order to build a Huffman tree that is optimal for a certain input text, one
needs to know how many times each input character occurs in the input text. In
other words, we need a map of every input character to a (non-zero) integer
frequency. We refer to such a map as an *alphabet*.

```
  type alphabet =
    (char, int) Hashtbl.t
```

Because the tree representation of the dictionary is perfectly suited while
decoding, we define a *decoding dictionary* to be a tree. While encoding, on
the other hand, it is more convenient to convert the dictionary to a more
convenient representation, namely a map of every input character to its
encoding. We refer to such a map as an *encoding dictionary*.

```
  type decoding_dictionary =
    tree

  type encoding_dictionary =
    (char, data) Hashtbl.t
```

The following functions can be useful to you while testing your code, and are
also used by the automated grading system, so their names can appear in some
of the messages produced by the grading system. `sort` sorts a list of
characters. `leaves` returns a list of all leaves of a tree, read from left to
right. `entries` returns a view of a hash table as a list of key-value pairs.

```
  val sort : char list -> char list
  val leaves : tree -> char list
  val entries : ('a, 'b) Hashtbl.t -> ('a * 'b) list
```

## Analyzing the Input Alphabet

Our first task, given an input text, is to compute its alphabet.

**Question 1.**
Write a function `build_alphabet`
of type `text -> alphabet`
such that
`build_alphabet text`
returns the alphabet of the input text `text`,
that is,
a map of every character that appears in `text` to
the number of times this character appears in `text`.

*Hint.* Use the standard library function `String.iter` to iterate over all
characters in the input text. Use the functions provided by the standard
library module `Hashtbl` to create, read, and update a hash table.

## Building an Optimal Tree: Huffman's Algorithm

Once we have an alphabet, a map of characters to frequencies,
we wish to compute a tree that is optimal with respect to this
alphabet.

Huffman's algorithm performs this task. This algorithm maintains
a set of pairs of a tree and an integer frequency.
It proceeds as follows:

* For each character `'c'` with frequency `freq` in the alphabet,
  create a one-leaf tree `Leaf c` and pair it with the frequency `freq`.

* As long as possible,
  pick two pairs `(tree0, freq0)` and `(tree1, freq1)`
  of minimal frequencies `freq0` and `freq1` out of the set.
  Build the composite tree `Node (tree0, tree1)` out of them,
  and insert it back into the set with frequency `freq0 + freq1`.

The algorithm stops where the set contains only a single pair `(tree, freq)`.
The tree `tree` is the desired tree. One can prove that it is optimal with
respect to the input alphabet.

In order to implement this algorithm in an efficient manner,
a priority queue is required. The OCaml standard library does
not provide one, but we do. Its signature is as follows:

```
  module BinomialQueue (X : sig
    type t
    val compare: t -> t -> int
  end) : sig
    type element = X.t
    type t
    val empty: t
    val is_empty: t -> bool
    val insert: element -> t -> t
    val extract: t -> element * t
  end
```

The functor `BinomialQueue` expects to be applied to a module that defines a
type `t` of elements, equipped with a total order. The function call `compare
x y` must return a negative result if `x` is less than `y`, zero if `x` and
`y` are equal, and a positive result if `x` is greater than `y`.

The functor `BinomialQueue` produces a module that defines a type `t` of
(immutable) priority queues. Four operations are provided. `empty` is the
empty queue. `is_empty` tests whether a queue is empty. `insert x q` is the
queue that contains the element `x` in the addition to the elements of the
queue `q`. `extract` must be applied to a nonempty queue `q`. It returns a
pair `(x, q')` where `x` is a *minimal* element of `q` (with respect to the
user-specified total order on elements) and `q'` is the queue `q` deprived of
`x`.

**Question 2.**
Define a module `Q` that provides priority queues whose
elements are pairs of a tree and an integer frequency. Drawing
an element of the queue must yield an element whose frequency
is minimal.
Then,
Define the function `build_tree`,
whose type is `alphabet -> tree`,
which implements Huffman's algorithm,
as described above.

## Building an Encoding Dictionary

As explained at the beginning of this exercise,
a tree defines a correspondence between input characters
and sequences of bits:
indeed,
a path from the root to a leaf corresponds to a bit string, where the bit
`0` means "go down towards the left child" and the bit `1` means "go down
towards the right child".

In order to encode data in an efficient manner,
we need a fast way of mapping each input character
to its encoding, a bit string.
In other words, we need an encoding dictionary:
a map of every input character to its encoding.

Such a map can be built in one traversal of the tree.

**Question 3.**
Define the function `build_encoding_dictionary`,
whose type is `tree -> encoding_dictionary`.

## Decoding Data

In order to interpret a sequence of bits as a character,
it suffices to view this sequence as a description of a
path in the tree. By descending along this path, one reaches
a leaf, which carries a character.

**Question 4.** Write a function `find` of type
`data -> int -> tree -> char * int`
such that `find data i t`
descends in the tree `t` along the path
that is encoded at index `i` in the binary data `data`,
until a leaf is reached,
and returns the character carried by that leaf
as well as an updated index into the binary data `data`.

## Encoding and Decoding the Decoding Dictionary Itself

Naturally, encoded data can be successfully decoded only
if the decoding dictionary is available. Thus, when one
wishes to store encoded data on disk or to transmit onto
the network, one must store or transmit not only the
encoded data, but also the decoding dictionary.

Because the decoding dictionary is a tree, it must first
be **serialized**, that is, transformed into binary data.

At the other end, whoever wishes to decode the data must
first **deserialize** the decoding dictionary, that is,
read its binary representation and convert it back to a
tree.

**Question 5.**
Complete the definitions of the functions `write`,
whose type is `tree -> data`,
and `read`,
whose type is `data -> tree * int`.

## Putting Everything Together

It is now time to put everything together
and define easy-to-use encoding and decoding
functions.

**Question 6.**
Define the functions `compress`,
whose type is `text -> data`,
and `decompress`,
whose type is `data -> text`.
They should be inverses of one another: that is,
`decompress (compress text)` should be `text`.
Compression should produce binary data: that is,
the string `compress text` should contain no characters
other than `'0'` and `'1'`.
Furthermore,
for sufficiently long and redundant input texts,
compression should save some space, that is,
the length of the string `compress text`
should be less than 8 times the length of `text`.

*Hint.* The data `compress text` must contain both the serialized decoding
dictionary and the encoded data.

*Note.* The factor of 8 in the length is due to the fact that each OCaml
character in the input text takes up 8 bits, whereas each `'0'` or `'1'`
character in the encoded text represents one bit.
