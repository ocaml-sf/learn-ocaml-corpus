# Evaluating a Game Tree: Minimax, Negamax, and Alpha-Beta

Consider a game of [Tic-Tac-Toe](https://en.wikipedia.org/wiki/Tic-tac-toe),
where the first player is named X and his opponent is named O.
Here are a few questions that one might ask about this game,
and their answers:

Is X assured to win?

* *No.*
  If both players play perfectly, the game must end in a draw.

Can X make a mistake on his first move?

* *No.*
  If X plays perfectly,
  no opening move by X can lead to a victory for O.

Assuming X plays his first move in a corner,
can O make a mistake on his first move?

* *Yes.*
  O must play his first move in the center,
  otherwise, assuming X plays perfectly,
  O will lose.

These questions, as well as many similar questions about Tic-Tac-Toe (and
about other two-player games where both players have full information about
the current state of the game) can be answered by an algorithm that
**evaluates** the **game tree**.

## Game Trees

A **game tree** is a tree where every node represents a state of the game and
every edge represents a move.

We assume that the game states (and therefore the tree nodes) can be separated
in two disjoint groups:

* The **terminal states** are those where the game is over.
  In a terminal state, no move is permitted.

* The **nonterminal states** are those where the game is not over.
  In a nonterminal state, at least one move is permitted.

The root of the tree represents the initial state of the game. Out of a node
that represents a game state *s*, for every move *m* that is permitted in state
*s*, there is an edge labeled *m* to a tree node that represents the game
state *s'* obtained by playing move *m* in state *s*.

We assume that *the two players take turns* in a strictly alternating manner,
that is, each player plays one move in turn. Thus, it is always clear which
player is up; this information does not need to be explicitly represented in
the tree.

In this exercise, we adopt an explicit representation of game trees as an
OCaml algebraic data type. This allows us to split the problem of evaluating a
game state as follows:

* The person who is responsible for **building a game tree**
  exploits purely game-specific knowledge,
  such as the encoding of game states in memory,
  the rules of the game, and so on.
  No knowledge of the (possibly complex) algorithms
  that are used to evaluate a tree is required.

* The person who is responsible for **evaluating a game tree**
  does so purely by exploring and reasoning about the tree.
  No knowledge of the underlying game is required.

The definition of game trees in OCaml is as follows:

```
type tree =
  | TLeaf of value
  | TNonLeaf of offspring

and offspring =
  (move * tree) Seq.t
```

A tree node is either a **leaf** node, which represents a terminal state,
or a **nonleaf** node, which represents a nonterminal state.

A leaf node carries an integer **value**,
which by convention represents the value of this game state
*in the eyes of the player who is currently up*.
Thus, for instance, the value +1 might represent a victory
for the current player, while the value 0 might represent a draw,
and the value -1 might represent a defeat for the current player.

Instead of just -1, 0, and +1, it is permitted to use arbitrary
integer values comprised between `bottom` and `top` (inclusive).
The choice of what value to use at a leaf node is up to whomever
builds a game tree.

The type `value` is defined as a synonym for `int`.
The values `bottom` and `top` are defined so that
`-bottom` is `top` and `-top` is `bottom`:

```
type value =
  int

let bottom, top =
  -max_int, max_int
```

A nonleaf node carries its **offspring**, that is, a nonempty sequence of
pairs of a permitted move and a subtree. This sequence enumerates all of
the permitted moves. Each move *m* in this sequence is accompanied with a
subtree whose root node represents the game state that this move leads to.

We assume that each move is somehow encoded as an integer. The details of
this encoding are up to whomever builds a game tree. (The permitted moves
need *not* be encoded as consecutive integers.)

```
type move =
  int
```

Because an offspring is represented as an OCaml *sequence*, as opposed to an
OCaml *list*, a game tree does not have to reside entirely in memory at once.
(The definition of OCaml sequences is recalled below.)
The parts of the tree that an evaluation algorithm needs to examine are built
on demand, and can be reclaimed by the garbage collector when they are no
longer needed. Thus, at a given moment, only one branch, namely the branch
that leads from the root of the tree down to the tree node under examination,
actually needs to reside in memory.

In this relatively simple-minded approach, there is *no sharing* of subtrees.
If two moves lead to the same game state, then the game tree contains two
identical subtrees. There is unfortunately no way of recognizing that these
subtrees are identical and (thereby) to avoid duplicated work. It is possible
to do so, using a hash table whose keys are game states, but this is not done
in this exercise.

## On-Demand Sequences

The type of **on-demand sequences** is defined in a module named `Seq`.
Beginning with version 4.07,
this module is part of OCaml's standard library.

```
module Seq : sig

  type 'a t = unit -> 'a node

  and +'a node =
  | Nil
  | Cons of 'a * 'a t

end
```

This data type is closely related to the algebraic data type of lists.
Indeed, if instead of `unit -> 'a node` one had written just `'a node`,
then this data type would have been isomorphic to the type of lists.

The presence of `unit -> ...` indicates that a sequence is in fact a function.
Calling this function, by applying it to the value `()`, amounts to requesting
the head of the sequence. This head can be either `Nil`, which means that the
sequence is empty, or `Cons (x, xs)`, which means that the first element of
the sequence is `x` and the remaining elements form another sequence `xs`. It
is worth noting that `xs` is itself a function, so the elements of the
sequence `xs` need not be explicitly computed until `xs` is applied.

## Warmup: Size and Height

As a warmup, let us implement two functions that compute the size and the
height of a game tree.

The **size** of a game tree is the number of its nodes (including both leaf
and nonleaf nodes).

The **height** of a game tree is the maximum length of a branch. In other
words, it is the maximum number of moves that can be played consecutively,
beginning in the initial game state.

**Question 1.** Define the function `size`,
whose type is `tree -> int`.

**Question 2.** Define the function `height`,
whose type is `tree -> int`.

## Evaluating a Game Tree: the Minimax Algorithm

Suppose that we are given a game tree. We wish to **evaluate** it, that is, to
compute the best possible outcome of the game for the player who is initially
up (the one who plays the first move), under the assumption that his opponent
plays perfectly. We refer to this player as `Even`, and refer to his opponent
as `Odd`. This is reflected in the following type definition:

```
type sense =
  | Even
  | Odd
```

The following auxiliary functions will be useful below. In particular,
`interpret sense` transforms a value that is relative to the player `sense`
to a value that is relative to the player `Even`. The value `unit sense` is
the neutral element of the binary operation `join sense`.

```
let opposite sense : sense =
  match sense with Even -> Odd | Odd -> Even

let interpret sense (v : value) : value =
  match sense with Even -> v | Odd -> -v

let join sense : value -> value -> value =
  match sense with Even -> max | Odd -> min

let unit sense : value =
  interpret sense bottom
```

What is the value in the eyes of the player `Even` of a game tree `t`,
under the assumption that, at the root of the tree, the player `Even` is up?

* If `t` is a leaf `TLeaf v`,
  then the player `Even` has no choice (the game is over),
  and the value of the tree is just `v`.

* If `t` is an internal node `TNonLeaf offspring`,
  where `offspring` is a sequence of pairs of a move and a subtree,
  then the player `Even` has a choice between these moves
  and can pick an optimal one,
  so the value is the maximum of the values in the eyes of the player `Even`
  of these subtrees *where the player* `Odd` *is now up*.

This leads us to ask a dual question.
What is the value in the eyes of the player `Even` of a game tree `t`,
under the assumption that, at the root of the tree, the player `Odd` is up?

* We let you answer this question by yourself!

**Question 3.** Define a function `eval` of type `sense -> tree -> value`
such that `eval sense t` computes the value in the eyes of the player `Even`
of the game tree `t`, under the assumption that, at the root of the tree, the
player `sense` is up.

*Hint.* In order to avoid code duplication between the case where `sense` is
 `Even` and the case where `sense` is `Odd`, make use of the auxiliary
 functions `opposite`, `interpret`, `join`, and `unit`.

*Note.* Because this code alternates between computing a maximum (at even
levels in the tree) and a minimum (at odd levels in the tree), it is known as
the [Minimax](https://en.wikipedia.org/wiki/Minimax) algorithm. The parameter
`sense` is so named because this is a typical name for a parameter that
alternates between two values.

*Note.* If the value of a game tree `t` in the eyes of one player is `v`, then
its value in the eyes of the other player is `-v`. Thus, the function `eval`
must satisfy the following law: `eval (opposite sense) t = -(eval sense t)`.

## Evaluating a Game Tree: the Minimax Algorithm in Negamax Style

Although it seems reasonable to parameterize the Minimax algorithm with a
`sense` parameter, as done above, there is a different formulation of the
algorithm, known as [Negamax](https://en.wikipedia.org/wiki/Negamax), where
this parameter is unnecessary.

This formulation relies on the remark that was made above:
`eval (opposite sense) t` is equal to `-(eval sense t)`. Thus, the function
`eval sense` can be formulated in such a way that every recursive call is of
the form `eval sense`, and no recursive call is of the form
`eval (opposite sense)`.
Therefore, the `sense` parameter is actually unnecessary! We can write a new
evaluation function, `nval`, which does not take such a parameter.

**Question 4.** Define a function `nval` of type `tree -> value` such that
`nval t` computes the value of the game tree `t` in the eyes of the player who
is up at the root of the tree.

*Note.* The connection between `nval` and `eval` is described by the following
law: `interpret sense (nval t) = eval sense t`. By replacing `sense` with
`Even` in this law, we find, in particular, `nval t = eval Even t`.

## Evaluating a Game Tree: the Minimax Algorithm in Negamax Tail-Recursive Style

In your answer to the previous question, you have likely defined the function
`nval_offspring` in a recursive manner, using an expression of the form `max
(...) (nval_offspring ...))`. Such an expression is certainly easy to read.
However, this style exhibits a few weaknesses:

* The maximum of the two values is computed *after* the recursive call.
  In other words, this is not a tail call.

* The OCaml programming language does not specify in what order the actual
  arguments are evaluated in a function call. Thus, it is not clear which of
  the two arguments of `max` is evaluated first. As a result, it is not clear
  in what order the subtrees are evaluated: in the order in which they appear
  in the sequence, or in the reverse order?

To address these shortcomings, it is preferable to organize the code in a
slightly different manner, where it is clear that the subtrees are evaluated
in the order in which they appear in the sequence, and where the recursive
call is a
[tail call](https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/data/tail_recursion.html).
This allows the OCaml compiler to produce code for a loop that iterates over
the sequence of subtrees.

**Question 5.** Define a function `ntval` of type `tree -> value` such that
`ntval t` computes the value of the game tree `t` in the eyes of the player
who is up at the root of the tree.

*Hint.* The auxiliary function `ntval_offspring` should now take two
parameters, namely `running_max`, the maximum value of the subtrees that have
already been examined, and `offspring`, the sequence of the subtrees that
remain to be examined.

*Hint.* The recursive call of `ntval_offspring` to itself should be a tail
call.

*Note.* The connection between `ntval` and `nval` is simple: `ntval t` must be
equal to `nval t`.

## Evaluating a Game Tree: the Alpha-Beta Algorithm in Negamax Style

Imagine that you are playing a game of chess. You suddenly discover that, if
your tower leaves the first row, then your opponent can move his own tower
into this row and checkmate. Do you ask yourself *what other moves* your
opponent might play instead? No: the fact that your opponent has a winning
move is sufficient grounds to stop examining this situation and recognize that
moving your tower away is a bad move.

This suggests that the value of a game tree can in some cases be determined
without examining the entire tree. Some subtrees can be *pruned*, that is,
never examined.

The
[Alpha-Beta](https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning)
algorithm improves on the Minimax algorithm by abandoning a sequence of
subtrees as soon as it is clear that their values are irrelevant. This yields
a dramatic performance improvement: it is an essential optimization.

As its name suggests, the Alpha-Beta algorithm is parameterized by two values,
`alpha` and `beta`, which must satisfy the strict inequality `alpha < beta`.
The algorithm is expected to evaluate a game tree `t` and is allowed to return
an *approximation* of the exact value `v` of `t`, as follows:

* If the value `v` lies strictly within the open interval `(alpha, beta)`,
  then the algorithm must return exactly `v`.

* If `v` lies within the closed interval `[bottom, alpha]`,
  then the algorithm may return any value in this interval.
  The idea is, if the value of the tree `t` is `alpha` or below `alpha`,
  then it is a *bad* tree in the eyes of the current player,
  and there is no need to find out exactly how bad it is:
  we will not play it.

* If `v` lies within the closed interval `[beta, top]`,
  then the algorithm may return any value in this interval.
  The idea is, if the value of the tree `t` is `beta` or above `beta`,
  then it is a *good* tree in the eyes of the current player,
  and there is no need to find out exactly how good it is:
  our opponent will not give us the opportunity of playing it.

In other words, the exact value of a game tree and the approximate value
computed by the Alpha-Beta algorithm must be `(alpha, beta)`*-equivalent* in the
following sense:

```
let equivalent alpha beta v1 v2 =
  v1 <= alpha && v2 <= alpha ||
  v1 = v2 ||
  beta <= v1 && beta <= v2
```

**Question 6.** Define a function `bval` of type `value -> value -> tree ->
value` such that `bval t` computes the value of the game tree `t` in the eyes
of the player who is up at the root of the tree, up to `(alpha,
beta)`-equivalence.

*Note.* The connection between `ntval` and `bval` is described by the
following law: `equivalent alpha beta (bval alpha beta t) (ntval t)`.
In particular, by instantiating `alpha` with `bottom` and `beta` with
`top`, we find the following law as a special case:
`bval bottom top t = ntval t`.
In other words, the Alpha-Beta algorithm computes the same result
as the Minimax algorithm
when the `(alpha, beta)` window covers the entire range of values.

*Hint.* The functions `bval` and `bval_offspring` are very similar to the
functions `ntval` and `ntval_offspring` defined earlier.

*Hint.* The parameter `alpha` in `bval_offspring` plays essentially the same
role as the parameter `running_max` in `ntval_offspring`, and is updated in
exactly the same way. Indeed, once we have found a subtree whose value is `v`,
we can view every value below `v` as *bad*. Because we are not interested in
distinguishing between bad values, we can update `alpha` to be at least `v`.

*Hint.* The parameter `beta` in `bval_offspring` allows pruning. If we find a
subtree whose value is `beta` or above `beta`, then this is *good*, and there
is no need to examine the rest of the sequence of subtrees. This is known as
a *cut*.

*Hint.* We keep working in Negamax style. So, when we make a recursive call to
*`bval`, we must provide values of `alpha` of `beta` that make sense *in the
eyes of the opponent.*

## Application: Finding and Playing a Winning Strategy

Recall the question that we asked at the very beginning: in a game of
Tic-Tac-Toe, is the first player assured to win? Suppose we are interested in
just a binary answer, that is, "yes" or "no". (If the answer is "no", we are
not interested in determining whether the second player is assured to win or
the game must end in a draw.) How can this question be efficiently answered
using the Alpha-Beta algorithm?

**Question 7.** Define a function `assured_win` of type `tree -> bool` such
that `assured_win t` is `true` if and only if the player who is up at the root
of the game tree `t` is assured to win, under the assumption that every leaf
in the tree `t` carries the value -1 (loss), 0 (draw), or +1 (win).

Being able to evaluate a game tree, and being able to tell that we assured to
win, is a good thing, but does not directly tell us *how* to win. Fortunately,
it is easy to define a variant of `bval` that returns not only the value of
the game tree, but also an optimal move.

A move is *optimal* if it achieves an optimal result in the eyes of the player
who is up. In other words, it is optimal if it leads to a subtree whose value
(in the eyes of the current player) is the same as the value of the entire
tree.

**Question 8.** Define a function `bmove` of type
`value -> value -> tree -> move option`
such that `bmove alpha beta t`
returns `None` if the game tree `t` is a leaf
and
returns `Some move` if the game tree `t` is of the form `TNonLeaf offspring`
and `move` is the first move in the sequence `offspring` that achieves the
optimal value `bval alpha beta t`.

## Comments

We have implemented algorithms that evaluate a game tree by exploring it
entirely, all the way down to its leaves (except of course where pruning is
possible). If one wishes to evaluate a game tree only **down to a limited
depth** `d`, this can be done, without modifying the evaluation algorithms
developed above, by pruning the tree at this depth, that is, by constructing a
shallower game tree where every node at depth `d` in the original tree is
replaced with a leaf. The value of this leaf is usually determined by a
heuristic *evaluation function* which, given a game state, attempts to guess
the value of this game state.

We have defined an explicit algebraic data type of game trees because this
allows us to abstract the game state away from the evaluation algorithm and to
test the evaluation algorithm with artificial game trees, independently of any
actual game. However, this simple and elegant approach has a cost. First,
allocating the game tree in memory imposes a penalty in terms of performance.
Second, and more seriously, hiding the game state from the evaluation
algorithm prevents us from making several crucial improvements to the
Alpha-Beta algorithm. For this reason, in a real-world implementation, the
evaluation algorithm would have direct access to game states, and no tree
would be explicitly built. The main improvements that could then be made are
as follows:

* **Exploiting sharing.** It is common for a game state to be reachable via
  different paths. Therefore, the state space of the game forms a *directed
  graph*. Representing this graph as a tree is correct, but is extremely
  inefficient, as this tree is typically exponentially larger than the
  underlying graph. In other words, when the state space of the game is
  represented as a tree, a single game state can appear in many places in the
  tree, and therefore can be evaluated many times. This leads to repeated
  work. In order to avoid this phenomenon, one must attempt to recognize that
  a game state has been reached and evaluated already. This is typically done
  by using a hash table, which in this case is usually known as a
  *transposition table*. Extending the Minimax algorithm with a transposition
  table is easy. Extending Alpha-Beta with a transposition table is slightly
  more difficult, as the values of `alpha` and `beta` are not necessarily the
  same when a game state is first visited and when it is visited again, but,
  with some care, this can be done. This yields an important gain in time, at
  a large cost in space.

* **Exploiting symmetries.** Some games exhibit symmetries. For instance, in
  Connect 4, taking the mirror image of the board (with respect to a vertical
  axis that goes through the center of the board) changes a game state to
  another game state whose value is the same. This remark allows dividing by 2
  the number of game states that must be evaluated. Similarly, in Tic-Tac-Toe,
  rotating the board by 90 degrees changes a game state to an equivalent game
  state. This remark allows dividing by 4 the number of game states that must
  be evaluated.

* **Exploiting move ordering.** In a given game state, several moves are
  usually permitted. In what order should these moves be explored? For the
  Minimax algorithm, this makes no difference. For the Alpha-Beta algorithm,
  however, it is desirable to explore better moves first, because this leads
  to more pruning, therefore better performance. Naturally, it is not known a
  priori which moves are best. One approach is to use a heuristic function,
  based on game-specific knowledge, to determine which moves seem most
  promising. Another approach is to exploit **iterative deepening**, that is,
  to run the Alpha-Beta algorithm successively down to some depth `d`, then
  down to depth `d+1`, then down to depth `d+2`, and so on. The approximate
  values obtained during one run are used during the next run to determine
  which moves look most promising. Because the cost of the last run alone
  usually dwarfs the combined cost of all previous runs, the cost of iterative
  deepening is not a problem; this technique is usually beneficial.
