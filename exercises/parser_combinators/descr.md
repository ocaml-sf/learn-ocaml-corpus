# Parser Combinators

In this exercise, on top of an implementation of the nondeterminism monad
(which is given), we build a set of **parser combinators** and use them to
build a parser for a simple language of arithmetic expressions.

## Parsers

A **parser** is an algorithm which recognizes the structure of a piece of text
and produces a convenient representation of this structure.

For instance, a parser for arithmetic expressions, applied to the string
`"1+2*3"`, would recognize that this sequence of characters represents an
addition whose left-hand operand is the literal `1` and whose right-hand
operand is the multiplication of the literals `2` and `3`. It would then
possibly return an *abstract syntax tree* (a data structure that records this
information in a natural way), or possibly evaluate this arithmetic expression
on the fly and return just the *integer value* `7`. It is up to us, when we
define a parser, to decide what type of result it returns.

A parser for arithmetic expressions, applied to the string `"1+2)"`, would
reject this ill-formed input. Thus, a parser can return zero results, or in
other words, *fail*.

The reason why we view the arithmetic expression `"1+2*3"` as an addition
(whose right-hand operand is a multiplication) is that we follow the usual
convention that "multiplicative operators have priority over additive
operators". In the absence of this convention, the input string `"1+2*3"`
would be *ambiguous*. A parser for arithmetic expressions, applied to this
string, would have to indicate that the value of this expression is either
`7` or `9`, depending on how it is interpreted. Thus, in general, a parser
can return *multiple results*.

These remarks suggest that a parser is a nondeterministic computation. Thus,
we can and should take advantage of the *nondeterminism monad* to facilitate
the implementation of parsers.

The nondeterminism monad is the topic of several other exercises, where it
is implemented in different ways,
using
<a href="" onclick="top.location='/exercises/nondet_monad_seq/';">sequences</a>,
using
<a href="" onclick="top.location='/exercises/nondet_monad_cont/';">continuations</a>,
and as an
<a href="" onclick="top.location='/exercises/nondet_monad_defun/';">abstract machine</a>.
All three implementations present the same API to the user.
In the following sections,
we explain the nondeterminism monad and its API.
We also recall the API of sequences, offered by the module `Seq`.
Then, we come back to parsers.

## The Nondeterminism Monad

When searching for the solution of a problem, one must typically
explore multiple choices. If a series of choices lead to a failure
(a dead end), then one must backtrack and explore another avenue.

There are a number of ways in which nondeterminism and backtracking
can be implemented. Regardless of which implementation mechanism is
chosen, it is desirable to hide it behind an abstraction barrier and
present the end user with **a simple API for constructing an executing
nondeterministic computations**.

This API is known as the **nondeterminism monad**. It offers the
following key elements:

* A type `'a m`, the type of computations that yield results of type
 `'a`.

* A number of constructor functions for constructing computations,
  such as `fail: 'a m`, which represents failure, and `choose: 'a m
  -> 'a m -> 'a m`, which expresses a nondeterministic choice
  between two computations.

* A single observation function, `sols: 'a m -> 'a Seq.t`, which
  converts a computation to a sequence of results, thereby allowing
  the user to execute this computation and observe its results. The
  name `sols` stands for `solutions`.

A monad can be thought of as a **mini-programming language** where
computations are first-class citizens: we have a type of
computations, ways of building computations, and a way of executing
computations.

A computation in the nondeterminism monad can produce zero, one, or
more results. Indeed, a computation that fails produces zero
results. A computation that succeeds normally produces one result.
A computation that uses `choose` can produce more than one result.
It is in fact possible to construct computations that produce an
infinite number of results!

## The Nondeterminism Monad's API

The signature, or API, of the nondeterminism monad is as follows:

```
module NonDet : sig

  (* Type. *)
  type 'a m

  (* Constructor functions. *)
  val return: 'a -> 'a m
  val (>>=): 'a m -> ('a -> 'b m) -> 'b m
  val fail: 'a m
  val choose: 'a m -> 'a m -> 'a m
  val delay: (unit -> 'a m) -> 'a m

  (* Observation function. *)
  val sols: 'a m -> 'a Seq.t

end
```

As explained above, a value of type `'a m` is **a description of a
computation**, which, once executed, produces a sequence of results
of type `'a`.

To execute a computation `m`, one must first convert it to a
sequence of type `'a Seq.t`, whose elements can then be demanded,
one by one. (More information on the module `Seq` is given below.)
This conversion is performed by the observation function `sols`.

The call `sols m` typically terminates in constant time; the actual
computation described by `m` takes place only when the elements of
the sequence `sols m` are demanded, and only insofar as necessary to
produce the elements that are demanded. For instance, applying
`Seq.head` to the sequence `sols m` forces the computation to
proceed up to the point where it is able to produce its first
result.

The constructor functions `return` and `(>>=)` exist in all monads.
(They are also known as `return` and `bind`.) `return` constructs a
trivial computation, which does nothing except return a value,
whereas `(>>=)` constructs the sequential composition of two
computations. Together, they allow constructing the sequential
composition of an arbitrary number of computations.

* The computation `return v` succeeds exactly once with the value
  `v`. In other words, the sequence of values that it produces is
  the singleton sequence composed of just `v`.

* The computation `m1 >>= m2` is the sequential composition of the
  computations `m1` and `m2`. This composition operator is
  asymmetric: whereas its first argument `m1` is a computation of
  type `'a m`, its second argument `m2` is a function of type `a ->
  'b m`. Every value `x` produced by `m1` is passed to `m2`,
  yielding a computation `m2 x`. The sequence of values produced by
  `m1 >>= m2` is the concatenation of the sequences of values
  produced by the computations `m2 x`, where `x` ranges over the
  values produced by `m1`.

The constructor functions `fail` and `choose` are specific of the
nondeterminism monad. `fail` can be thought of as a 0-ary
disjunction, whereas `choose` is a binary disjunction. Together,
they allow constructing the disjunction of an arbitrary number of
computations.

* The computation `fail` returns no result. In other words, it
  produces an empty sequence of values.

* The sequence of values produced by `choose m1 m2` is the
  concatenation of the sequences of values produced by `m1`
  and by `m2`.

The constructor function `delay` is not exploited in this exercise on parser
combinators. Although we could implement a `delay` parser combinator (and, in
a real-world implementation of parser combinators, this combinator would be
useful), in this exercise, we won't.

## The `Seq` API

The type of **on-demand sequences** is defined in a module named `Seq`.
Beginning with version 4.07,
this module is part of OCaml's standard library.

```
module Seq : sig

  type 'a t = unit -> 'a node

  and +'a node =
  | Nil
  | Cons of 'a * 'a t

  val nil : 'a t
  val cons: 'a -> 'a t -> 'a t
  val singleton: 'a -> 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val concat: 'a t -> 'a t -> 'a t
  val flatten: 'a t t -> 'a t

  val take: int -> 'a t -> 'a t

  val head: 'a t -> 'a option

  val of_list: 'a list -> 'a t
  val to_list: 'a t -> 'a list

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

Sequences are closely related to *iterators* in object-oriented languages,
such as C++ and Java. Yet, sequences are much simpler than iterators, for
two reasons:

* they involve no mutable state;

* they are just as easy to construct and to use as ordinary lists.

The functions `nil`, `cons`, and `singleton` are constructor functions.

The functions `map`, `concat`, `flatten` are analogues for sequences
of the standard list functions `List.map`, `(@)`, and
`List.flatten`.

The function `Seq.take` truncates a sequence at a certain length:
`Seq.take n xs` is a sequence that begins like `xs` but has at most
`n` elements.

The function `Seq.head` demands the first element of a sequence. If
the sequence begins with an element `x`, then `Some x` is returned;
otherwise, `None` is returned. This forces enough computation to
take place so as to be able to produce the first element of the
sequence.

The functions `Seq.of_list` and `Seq.to_list` convert between lists
and sequences, both ways. One must keep in mind that applying
`Seq.to_list` to a sequence `xs` causes all of its elements to be
demanded: that is, it forces all of the suspended computations to
take place. In particular, if `xs` is an infinite sequence, then
`Seq.to_list xs` does not terminate.

## What's a Parser?

As explained earlier, a parser reads a piece of input text and produces zero,
one, or more results, where each result represents a possible interpretation
of this text.

We wish to construct parsers in a compositional way: that is, we wish to
construct complex parsers by combining simpler parsers. Thus, we must allow
for the possibility that a parser does not necessarily read *all* of the
input, from beginning to end, but reads only a *fragment* of the input. In
other words, if a **cursor** is a pointer into the input text, then a parser
must receive an initial cursor as an argument. Furthermore, instead of
returning just a number of *results*, a parser must return a number of *pairs
of a result and a final cursor*, so that we can tell how far it has
progressed.

This leads us to the following type definition:

```
type 'a parser =
  cursor -> ('a * cursor) NonDet.m
```

Thus, a parser is a function which, when applied to an initial cursor,
constructs a nondeterministic computation which, when executed, produces a
number of pairs of a result and a final cursor.

Now, what is a cursor? It turns out that we need only one operation on
cursors, namely reading the character that follows the cursor, if there is
one. This operation takes a cursor and returns either a pair `(c, cursor)` of
a character and a new cursor (which points one character further than the
original cursor) or nothing (which means that the end of the input stream has
been reached).

This remark implies that a cursor *is* a sequence of characters: indeed, a
sequence, as provided by the module `Seq`, is precisely a representation of
sequences that offers just one way of observing a sequence, by demanding its
first element.

```
type token =
  char
type cursor =
  token Seq.t
```

For simplicity, we assume that the input is a sequence of characters. More
generally, it would be possible to define parsers that accept sequences of
**tokens**, where the type of tokens is chosen by the user.

## Executing a Parser

How does one run a parser `p`?
First, the input string `input` must be converted to a sequence
of characters, therefore, a cursor.
Applying the parser `p` to this initial cursor
yields a computation in the `NonDet` monad.
Running this computation,
by applying `NonDet.sols`,
yields a sequence of pairs of a result and a final cursor.

Assuming that we are not interested in the final cursors,
we discard them, by applying `Seq.map fst`.
This yields a sequence of results.
(If the parser `p` has been constructed in such a way that it can
succeed only at the end of the input stream,
then every final cursor must be an empty sequence of characters,
so no information is lost by discarding it.)

The code for executing a parser is as follows:

```
let run (p : 'a parser) (input : string) : 'a Seq.t =
  input
  |> Seq.of_string
  |> p
  |> NonDet.sols
  |> Seq.map fst
```

Implementing `run` could have been the first question of this exercise. We
prefer to give it and start on a sound basis. The function `run` is used by
the automatic grader: if your code does not behave as expected, you will see
an error message where `run` appears.

## The Parser Monad

In short, a parser is a nondeterministic computation that maintains a current
state, namely, the current cursor.

Because a parser is a computation, it should not come as a surprise that
**parsers form a monad**. That is, the type `'a parser` can be equipped with
the basic combinators `return`, `>>=`. In fact, all of the
constructor functions offered by the nondeterminism monad can be lifted up to
the level of the parser monad. Parsers offer the following API:

```
  (* Type. *)
  type 'a parser

  (* Observation function. *)
  val run: 'a parser -> string -> 'a Seq.t

  (* Constructor functions. (The Monad API.) *)
  val return: 'a -> 'a parser
  val (>>=): 'a parser -> ('a -> 'b parser) -> 'b parser

  (* Constructor functions. (The Applicative Functor API.) *)
  val map: ('a -> 'b) -> 'a parser -> 'b parser
  val (<&>): 'a parser -> 'b parser -> ('a * 'b) parser
  val (>>): 'a parser -> 'b parser -> 'b parser
  val (<<): 'a parser -> 'b parser -> 'a parser

  (* Constructor functions. (The Nondeterminism API.) *)
  val fail: 'a parser
  val choose: 'a parser -> 'a parser -> 'a parser
  val at_most_once: 'a parser -> 'a parser

  (* Constructor functions. (The Parser API.) *)
  val any: token parser
  val eof: unit parser
  val char: token -> token parser
```

The above constructor functions are also known as **parser combinators**.

The first group of combinators form the *Monad* API. The meaning of each
combinator can be briefly described as follows:

* The parser `return x` consumes no input (that is, it does not move the
  current cursor) and returns exactly one result, namely `x`.

* The parser `p >>= q` is the sequential composition of `p` and `q`. The
  parser `p` is executed first. If (and every time) `p` produces a result `x`
  and advances the current cursor, the parser `q x` is executed, thereby
  producing a final result and further advancing the cursor.

The next group of combinators form the *Applicative Functor* API. These
combinators can in fact be implemented in terms of `return` and `>>=`.
(In other words, "every monad is an applicative functor".)

* The parser `map f p` works like `p`. In addition, the function `f` is used
  to transform every result produced by `p`.

* The combinators `<&>`, `>>`, and `<<` are special cases of the
  sequential composition combinator `>>=`. It is not difficult to guess,
  based on their types, how they are supposed to behave.

The following group form the *Nondeterminism* API. These combinators make
sense in a setting where a computation can produce zero, one, or more results.

* The parser `fail` always fails.

* The parser `choose p q` represents an alternative between the parsers `p`
  and `q`. The sequence of result/cursor pairs that it produces is the
  concatenation of the sequences produced by `p` and `q` separately.

* The parser `at_most_once p` works like `p`, but produces at most one result.
  Thus, if `p` fails, then `at_most_once p` fails, too.
  If `p` produces a result, then `at_most_once p` produces this result, too,
  and commits to it: any further results that `p` could produce are ignored.

The last group of combinators are specific of parsers; they form the *Parser* API.

* The parser `any` returns the next character (that is, the character at the
  current cursor) and consumes this character (by advancing the cursor). If
  the cursor is at the end of the input stream already, then `any` fails.

* The parser `eof` does not move the cursor. If the cursor is at the end of the
  input stream, then it returns `()`. Otherwise, it fails.

* The parser `char c` tests whether the next character is the character `c`.
  If so, it consumes this character (by advancing the cursor). Otherwise, it
  fails.

**Question 1.** Implement all of the constructor functions listed above.

## Recursion

A *recursive* parser is a parser whose definition refers to itself. In other
words, since a parser is a function of a cursor to a nondeterministic
computation, a recursive parser is just a recursive function.

Recursive parsers are extremely common and useful. One must be careful,
though, to ensure termination. If a parser `p` applied to a certain cursor
causes a recursive call to `p` *at the very same cursor*, then the program
will not terminate (or will terminate abruptly by exhausting the space
allotted to the stack). In technical speak, **left recursion is forbidden**. A
cycle of recursive calls is permitted only if, somewhere along the cycle, the
current cursor is advanced.

As an example of a valid recursive definition, we now wish to define the
parser combinators `star` and `plus`.

The parser `star p` accepts the concatenation of *any number* of strings
accepted by the parser `p`. The parser `plus p` accepts the concatenation of
*any nonzero* number of strings accepted by the parser `p`.

As their names suggest, these combinators are analogous to the iteration
constructs `p*` and `p+` that are standard in regular expressions. However,
the parsers `star p` and `plus p` do *not* have "longest-match" semantics:
they accept *any* number of repetitions. Thus, they will often succeed in
*more than one* way.

In the definition of `star p` and `plus p`, it is permitted to assume that the
parser `p` does *not* accept the empty string. In other words, the parser `p`
*must* consume a nonempty fragment of the input. In other words still, it
*must* advance the cursor. This guarantees that the (mutual) recursion in the
definitions of `star` and `plus` is well-founded.

In the following question, we use `star`, `plus`, and other parser combinators
in order to build parsers that recognize (decimal) digits and numbers. A
**digit** is a character in the range `'0'..'9'`. A **number** is a nonempty
sequence of digits.

**Question 2.** Define the parser combinators `star` and `plus`, whose type is
`'a parser -> 'a list parser`. Define a parser `digit` of type `int parser`
that recognizes a digit and returns its value as an integer in the range
`0..9`. Define a parser `number_lax` of type `int parser` that recognizes a
digit sequence *of arbitrary nonzero length* and returns its interpretation as
an integer. Define a parser `number` of type `int parser` that recognizes a
nonempty digit sequence *of maximal length* and returns its interpretation as
an integer.

## Parsing Arithmetic Expressions

In the last part of this exercise, we would like to parse and evaluate
arithmetic expressions. We begin with a very simple language where only
integer literals, addition, and subtraction are permitted (Question 3).
Then, we add multiplication, division, and the ability to enclose a
subexpression within parentheses (Question 4).

In the following, `'a op` is an abbreviation for the type of a binary
operation whose operands and result have type `'a`:

```
type 'a op =
  'a -> 'a -> 'a
```

The following three unnumbered questions are not explicitly graded.
Question 3, which relies on them, is graded, so you will receive
feedback after you complete Question 3.

**Question.** Define a parser `additive_op` of type `int op
parser` that recognizes one of the characters `'+'` or `'-'` and
interprets it as the corresponding operation on integers.

**Question.** Define a parser `multiplicative_op` of type `int op
parser` that recognizes one of the characters `'*'` or `'/'` and
interprets it as the corresponding operation on integers.

**Question.** Define a parser combinator `chainl1` of type
`'a parser -> 'a op parser -> 'a parser` such that the parser
`chainl1 p op` recognizes a nonempty sequence of `p`'s,
separated with `op`'s,
and interprets it as a left-associative chain of applications
of operators to values.

**Question 3.** Define a parser `sum` of type `int parser` that recognizes and
evaluates a **sum**, that is, a nonempty sequence of numbers, separated by
additive operators. For instance, the string `"24-12-12"` should be evaluated
to the single integer result `0`.

We now wish to introduce the multiplicative operators as well as the ability
to enclose a subexpression within parentheses. In other words, we would like
to parse the language described by the following **context-free grammar**:

```
  expr ::= number
         | expr additive_op expr
         | expr multiplicative_op expr
         | '(' expr ')'
```

Unfortunately, this grammar cannot directly be used as a guide in the
definition of a parser. Indeed, it exhibits several flaws. To begin with, it
is ambiguous: it does not indicate that every operator should be considered
left-associative, nor does it indicate that multiplicative operators have
greater priority than additive operators. Furthermore, it is left-recursive:
several right-hand sides in the definition of `expr` begin with `expr` itself.
A parser whose structure is modeled directly after this grammar would diverge.

In order to work around these problems, a common solution is to use a slightly
more complex grammar, where three syntactic categories are distinguished,
namely **atoms**, **factors**, and **terms**:

```
  atom   ::= number
           | '(' term ')'
  factor ::= atom ( additive_op atom )*
  term   ::= factor ( multiplicative_op factor )*
```

This grammar can be transcribed in English as follows:

* An **atom** is either a number or a term enclosed in parentheses.

* A **factor** is a nonempty sequence of atoms, separated with additive operators.

* A **term** is a nonempty sequence of factors, separated with multiplicative operators.

This grammar is still recursive, because `term` depends on `factor` which
depends on `atom` which depends on `term`. However, it is not left-recursive:
along this cycle, at least one input symbol (namely, an opening parenthesis)
must be consumed.

**Question 4.** Define a parser `term` of type `int parser` that recognizes
and evaluates a **term**, as defined by the above grammar. For instance, the
string `"49-12*(2+2)"` should be evaluated to the single integer result `1`.

*Hint.* One approach is to define `atom`, `factor` and `term` as three
mutually recursive functions. An alternative approach is to use the parser
combinator `fix`, whose type is `('a parser -> 'a parser) -> 'a parser`,
and whose definition is given to you.

## Notes

The parser monad, and parser combinators, are often considered an attractive
approach to parsing, for several reasons:

* Because parsers are ordinary functions, they are **notationally
  lightweight**. In particular, because recursive and mutually recursive
  parsers can be easily defined, **a grammar can be directly transcribed as a
  parser** of identical structure.

* Because parsers are ordinary functions, it is easy to define highly
  parameterized, **highly reusable** parser combinators, such as `star`,
  `plus`, and `chainl1` in this exercise.

*Note.* The first argument above works better in Haskell than in OCaml. In
OCaml, the right-hand side of a recursive definition must be a syntactic
function `fun ...` or a lazy thunk `lazy ...`, which makes things more
verbose: unpleasant eta-expansions are often required. Also, in OCaml,
explicit uses of the `delay` combinator may be required, whereas in Haskell,
every computation is suspended by default.

That said, parser combinators, as presented here, also have several deep
limitations:

* They **do not support left recursion**, which causes an infinite loop.

* They **do not guarantee unambiguity**. In other words, it can be very
  difficult to convince oneself, by inspection of the code, that a parser
  can produce at most one result.

* They **do not guarantee linear time complexity**. This is related to
  the previous point: because a parser is a nondeterministic computation,
  its execution involves backtracking. In degenerate cases, it
  can require **exponential time** in the length of the input.

* Although parser combinators do guarantee linear space complexity,
  in practice,
  **their space requirement can be very large**.
  This is again due to nondeterminism:
  the data structures that are required in order to support backtracking,
  known as *choice points*,
  tend to accumulate
  even when they are not needed,
  because it is not statically known which computations may or may not
  produce more than one result.
  Explicit uses of the combinator `at_most_once` can remedy this to some extent,
  but placing them correctly requires expertise and effort.

It is possible to try and remedy some of the above limitations by performing
aggressive *memoization*, as suggested in the paper
[Practical, general parser combinators](https://ir.cwi.nl/pub/25145/25145.pdf)
by Izmaylova, Afroozeh and van der Storm (2016). This is technically quite
impressive, but also quite complex, and still does not offer guaranteed
unambiguity.

We wrote above that "a parser is a nondeterministic computation that maintains
a current state, namely, the current cursor". In fact, our Parser monad can be
viewed as an application of the
[State monad transformer](https://en.wikibooks.org/wiki/Haskell/Monad_transformers)
to the nondeterminism monad,
extended with parser-specific operations such as `any` and `eof`.
We could have taken advantage of this remark to make our code more
modular and more abstract. For the sake of simplicity,
we preferred to keep it more concrete.

In summary, we have presented parser combinators because they constitute **a
good programming exercise** and a nice illustration of **nondeterministic
programming in the presence of state**. That said, as a parsing technique,
they have problematic limitations. If you wish to build a parser for a
specific language, we recommend looking into other parsing techniques, such as
LR(1) or SGLR, depending on your specific needs. We also recommend using an
existing **parser generator** instead of writing your own parsing code, as a
mature dedicated tool can offer a range of services that is difficult to
match.
