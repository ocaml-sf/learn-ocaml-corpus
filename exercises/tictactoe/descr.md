# Building a Game Tree

In this exercise, we build a **game tree** for the game of
[Tic-Tac-Toe](https://en.wikipedia.org/wiki/Tic-tac-toe).
This allows us to use the Alpha-Beta algorithm (developed in a
<a href="" onclick="top.location='/exercises/alpha_beta/';">companion
exercise</a>) to evaluate this tree and construct an optimal playing
strategy.

In the original game of Tic-Tac-Toe, the players use a board of width 3 and
height 3, and the first player who aligns 3 marks (horizontally, vertically,
or diagonally) wins. We generalize the game as follows: the game is played on
a board of width `w` and height `h`, and a winning **alignment** is a series
of `k` consecutive marks on a row, a column, or a diagonal.

In the first part of this exercise, we momentarily set aside the game of
Tic-Tac-Toe and concentrate on representing a game board as a bitmap. In the
second part of this exercise, we come back to Tic-Tac-Toe, choose a
representation of the game state, and build a game tree.

## Representing a Game Board as a Bitmap

We are interested in representing in memory a game board whose width is `w`
and whose height is `h`. We suppose, for now, that there is only one kind of
mark, so that each square on the board is either empty or marked. A board is
then a two-dimensional Boolean matrix whose dimensions are `w` and `h`.

One could represent such a matrix in memory as an array of Booleans whose size
is `w * h`. However, that would be quite costly.
Because every OCaml value occupies one word of memory,
an array of `w * h` Boolean values occupies
`w * h` words of memory.
On a modern machine, one word is 64 bits.
Thus, this representation is 64 times less compact than it could be!
This entails a waste of space
and (therefore) a waste of time.

For these reasons, we prefer to pack this matrix into a single **integer**
value, so that each entry in the matrix is represented by **one bit**. We
refer to this representation as a **bitmap**, and define the type `bitmap`
as a synonym for `int`. Quite obviously, this
representation is much more compact than an OCaml array of Booleans.
Furthermore, it opens the door to the use of **bit-level parallelism**, that
is, the use of machine instructions that operate on all bits at once. This is
useful in the implementation of certain operations, such as the detection of
an alignment.

In order to facilitate the detection of an alignment, it is
convenient to leave an empty row on top of the matrix and an empty column on
its right-hand side. Therefore, a matrix whose dimensions are `w + 1` and `h +
1` is required.

As long as the product `(w + 1) * (h + 1)` is strictly less than the word size
(that is, 64), such a matrix can be represented as a bitmap within a single
OCaml integer value.

By convention, the bits in an OCaml integer value are numbered from `0`
(included) to `63` (excluded). The least significant bit is numbered 0.

We adopt the convention that the board square whose coordinates are `i` and
`j` is encoded by the bit whose number is `(h + 1) * i + j`. Here, `i` is a
column index, and ranges from `0` (included) to `w` (included), while `j` is a
row index and ranges from `0` (included) to `h` (included).

The function `encode`, which we have defined for you, converts a pair of
indices `i` and `j` to an **offset**, that is, a bit number. The expression
`encode w h i j` is synonymous with `(h + 1) * i + j`. The type `offset` is
a synonym for `int`.

According to this convention, a 3x3 board occupies 16 bits,
and is represented as follows:

```
   .  .  . .
   2  6 10 .
   1  5  9 .
   0  4  8 .
```

This drawing means that the bits 0, 1, 2 encode the leftmost column;
the bits 4, 5, 6 encode the central column; and
the bits 8, 9, 10 encode the rightmost column.
By convention, the bits 3, 7, 11, 12, 13, 14, 15
encode the extra empty row and column mentioned above;
their value is always zero.

Thus, a 3x3 board where every square is marked is represented by the bitmap
`0000011101110111`, that is, the integer value `0b0000011101110111`, which in
decimal notation is written `1911`. A 3x3 board where only the leftmost column
is marked is represented by the bitmap `0000000000000111`, that is, by the
integer 7. A board where only the bottom row is marked is represented by the
bitmap `0000000100010001`, that is, by the integer 273. An empty board is
represented by the bitmap `0000000000000000`, that is, by the integer 0.

For convenience, we provide a function `bitmap` of type `int -> int -> string
-> bitmap` such that `bitmap w h` converts a string representation of a bitmap
to an actual bitmap. The string must contain `(w + 1) * (h + 1)` characters,
each of which must be `'0'` or `'1'`. (Additional whitespace characters can be
present, and are ignored. The character `'.'` is interpreted as `'0'`.) Thus,
a 3x3 board where every square is marked can be obtained as follows:

```
  bitmap 3 3 "
    ....
    111.
    111.
    111.
  "
```

You can use the Toplevel pane to check that this expression produces the value
`1911`, as expected. You do *not* need to use the function `bitmap` in your
code. The function `bitmap` can appear in the messages produced by the
automatic grading system for this exercise.

In the text so far, we have distinguished between a board (an abstract object
that we have in mind) and a bitmap (a representation of this object in
memory), and we have repeatedly written that a bitmap *represents* a board. In
the following, however, maintaining this careful distinction would lead us to
write rather convoluted sentences, so we abandon this distinction and use the
words "bitmap" and "board" interchangeably.

**Question 1.** Define a function `decode` of type `int -> int -> offset ->
int * int` such that `decode w h` is the inverse of `encode w h`, that is,
`decode w h` maps an offset back to a pair of a column index `i` and a row
index `j`.

**Question 2.** Define a function `mask` of type `int -> int -> int -> int ->
  bitmap` such that, in the board `mask w h i j`, the square at `(i, j)` is
  marked, and every other square is unmarked.

*Hint.* For instance, in the board `mask 3 3 1 1`, only the square at (1, 1)
must be marked. According to the above drawing, the square at (1, 1) is
represented by the bit number 5, so `mask 3 3 1 1` should yield the integer
value `0b0000000000100000`, or in other words, the integer value 32.

*Hint.* Use `encode` and the
*[logical shift left](https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift)* operator `lsl`.

**Question 3.** Define a function `read` of type `int -> int -> bitmap -> int
-> int -> bool` such that `read w h bitmap i j` is `true` and if only if the
square at `(i, j)` is marked in the board `bitmap`.

*Hint.* Use `mask` and the
*[logical and](https://en.wikipedia.org/wiki/Bitwise_operation#AND)* operator `land`.

**Question 4.** Define a function `update` of type `int -> int -> bitmap ->
int -> int -> bitmap` such that the board `update w h bitmap i j` is marked
at `(i, j)` and is identical to the board `bitmap` elsewhere.

*Hint.* Use `mask` and the
*[logical or](https://en.wikipedia.org/wiki/Bitwise_operation#OR)* operator `lor`.

Recall that a `k`-**alignment** is a series of `k` consecutive marks along a
row, a column, or a diagonal.

There exist four kinds of alignments. In other words, there are four
**directions** along which an alignment can exist: *north*, *east*,
*northeast*, and *southeast*. (We make an arbitrary choice between
*north* and *south*, as there is no need to consider both; similarly
for the other directions.)

Each direction can be represented by a positive *offset*. Indeed, recall how a
3x3 board is represented:

```
   .  .  . .
   2  6 10 .
   1  5  9 .
   0  4  8 .
```

Adding 1 to a bit number corresponds to taking one step towards the north.
Thus, the direction *north* is represented by the offset 1. For this reason,
we define the auxiliary function `north` by `north w h = 1`.

**Question 5.** Define the auxiliary functions `east`, `northeast`, and
`southeast` in a similar manner.

We wish to be able to efficiently detect the existence of an alignment
anywhere on the board. Fortunately, bit-level parallelism is our friend: the
bitwise operations (`lsr`, `lsl`, `land`, `lor`, `=`, etc.) operate on all
bits at once, and are extremely cheap. It turns out that, with a small number
of such operations, one can detect the presence of an alignment anywhere on
the board.

The idea is as follows:

* The board, which we know, is the set of all squares
  such that there is a mark on this square.

* In *one* well-chosen bitwise operation, one can compute the set
  of all squares
  such that there is a mark to the north of this square.

* In *one more* well-chosen bitwise operation, one can combine the
  above two bitmaps and compute the set of all squares
  such that there is a mark on this square
  and a mark to the north of this square.
  In other words, this is the set of all squares such that there is a
  northern 2-alignment that begins at this square.

* In just *two more* bitwise operations,
  one can compute the set of all northern 4-alignments.

* This process can be generalized to any
  value of `k` and to any of the four directions.

We define the type `direction` as a synonym for `int -> int -> offset`,
so the functions `north`, `east`, `northeast` and `southeast` have type
`direction`.

**Question 6.** Define a function `alignments` of type
`int -> int -> int -> direction -> bitmap -> bitmap`
such that `alignments w h k direction bitmap`
is the set of all squares such that there is a `k`-alignment
along `direction` that begins on this square.

*Hint.* Give a recursive definition of `alignments`. The base case can be 0
or 1. The recursive structure can be chosen so that the runtime complexity of
`alignments` is O(*log* k).

**Question 7.** Define a function `has_alignment` of type
`int -> int -> int -> bitmap -> bool`
such that `has_alignment w h k bitmap` is `true` if and only if
there exists a `k`-alignment, along any of the four directions,
anywhere on the board.

## Representing the Game State of Tic-Tac-Toe

In a [(w, h, k)-game](https://en.wikipedia.org/wiki/M,n,k-game), the
parameters `w` and `h` are the width and height of the board, while the
parameter `k` is the size of an alignment that a player must achieve in order
to win. The parameters `w`, `h`, `k` remain fixed throughout an entire game.
In the game of Tic-Tac-Toe, they are equal to 3.

We represent a game state in OCaml as a record with the following fields:

```
  type state = {
    w: int; h: int; k: int;
    player_bitmap: bitmap;
    opponent_bitmap: bitmap;
    past_moves: int;
  }
```

The current state of the board is represented by *two bitmaps*. The bitmap
`player_bitmap` stores the marks of the player who is currently up, while the
bitmap `opponent_bitmap` stores the marks of his opponent.

The integer field `past_moves` stores the number of moves that have been
played. This field is redundant, as its value can be recovered by counting how
many bits are set in the bitmaps `player_bitmap` and `opponent_bitmap`.
Nevertheless, it is convenient to have this field: this removes the need for
an efficient implementation of the "bit count" operation.

It is now up to you to write a number of simple auxiliary functions that
operate on game states, namely `initial`, `square_is_available`, `play`,
`no_square_is_available`, and `opponent_has_won`. (No automatic grading
is performed at this stage.) Then, you can move on to the final question:

**Question 8.** Define a function `tree` of type `state -> tree`
such that `tree state` is the game tree associated with
the game state `state`.

*Note.* The definition of a game tree is explained in the
<a href="" onclick="top.location='/exercises/alpha_beta/';">companion
exercise</a>
on the Alpha-Beta algorithm.

*Hint.* In a game tree, the value of a leaf is always relative to the player
who is currently up. This value should be +1 if this player has won, 0 if the
game is a draw, and -1 if this player has lost.

*Hint.* In Tic-Tac-Toe, as in many common games, there cannot be a leaf whose
value is +1. That would mean that our opponent has just played, yet we have
won. This is impossible: no move can cause a player to lose immediately.

*Hint.* The functions `Seq.interval`, `Seq.filter`, and `Seq.map` can be
 helpful in the construction of a sequence of permitted moves.

## To Go Further

If you have completed both this exercise and its <a href=""
onclick="top.location='/exercises/alpha_beta/';">companion exercise</a> on the
Alpha-Beta algorithm,
then we recommend combining them to solve Tic-Tac-Toe.
You can verify that, if both players play perfectly,
then the game must end in a draw.
Although the game tree for Tic-Tac-Toe has size 549946,
the Alpha-Beta algorithm evaluates it in a fraction of a second.
(The algorithm builds and explores only part of the tree.)

To explore larger values of `w`, `h`, and `k`, such as (4, 4, 4),
it is necessary to improve the Alpha-Beta algorithm; in particular,
a transposition table is required.
