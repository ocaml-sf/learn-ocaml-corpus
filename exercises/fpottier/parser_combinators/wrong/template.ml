(* The type [parser] satisfies the monad API: [return], [>>=]. *)

(* [return a] always succeeds, consumes no input, and returns [a]. *)

let return (a : 'a) : 'a parser =
  (* TO DO: Define this function. *)
  raise TODO

(* [p >>= f] is the sequential composition of the parsers [p] and [f]. *)

let (>>=) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The type [parser] supports the nondeterminism monad API: [fail], [choose],
   [at_most_once]. *)

(* The parser [fail] accepts the empty language: it always fails. *)

let fail : 'a parser =
  fun cursor ->
    (* TO DO: Define this function. *)
    raise TODO

(* The parser [choose p q] accepts the union of the languages accepted
   by the parsers [p] and [q]. *)

let choose (p : 'a parser) (q : 'a parser) : 'a parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The parser [at_most_once p] accepts the same language as [p], but
   accepts each input string in at most one way. *)

let at_most_once (p : 'a parser) : 'a parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The type [parser] supports the applicative functor API: [map], [<&>]. *)

(* [map f p] accepts the same language as the parser [p], and applies the
   function [f] to every result produced by [p]. *)

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The parser [p <&> q] accepts the concatenation of the languages accepted
   by the parsers [p] and [q]. For each value [a] returned by [p] and for
   each value [b] returned by [q], it returns the pair [(a, b)]. *)

let (<&>) (p : 'a parser) (q : 'b parser) : ('a * 'b) parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The parser [p >> q] accepts the concatenation of the languages accepted
   by the parsers [p] and [q]. For each value [a] returned by [p] and for
   each value [b] returned by [q], it returns just [b]. *)

let (>>) (p : 'a parser) (q : 'b parser) : 'b parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The parser [p << q] accepts the concatenation of the languages accepted
   by the parsers [p] and [q]. For each value [a] returned by [p] and for
   each value [b] returned by [q], it returns just [a]. *)

let (<<) (p : 'a parser) (q : 'b parser) : 'a parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The type [parser] supports a number of operations that are specific of the
   parser combinator monad. The most basic two are [any] and [eof]. On top of
   these two, more combinators are defined, including [sat], [char], [digit],
   and so on. *)

(* [any] succeeds if and only if the cursor is currently not at the end of the
   input stream. It consumes and returns the next input token. *)

let any : token parser =
  fun cursor ->
    (* TO DO: Define this function. *)
    raise TODO

(* [eof] succeeds if and only if the cursor is currently at the end of the
   input stream. It consumes nothing, and returns [()]. It is idempotent:
   [eof >> eof] is the same as [eof]. *)

let eof : unit parser =
  fun cursor ->
    (* TO DO: Define this function. *)
    raise TODO

(* The parser [sat p] consumes and returns the next input token if this
   token satisfies the predicate [p]. It fails otherwise. *)

let sat (p : token -> bool) : token parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The parser [char c] consumes and returns the next input token if this
   token is the character [c]. It fails otherwise. *)

let char (c : token) : token parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The parser [digit] consumes the next token if this token is a character
   in the range ['0'..'9']. It returns an integer between 0 and 9. *)

let is_digit c : bool =
  (* TO DO: Define this function. *)
  raise TODO

let decode_digit c : int =
  (* TO DO: Define this function. *)
  raise TODO

let digit: int parser =
  (* TO DO: Define this parser. *)
  fail

(* Iteration. *)

(* The parser [star p] accepts the concatenation of any number of strings
   accepted by the parser [p]. The parser [plus p] accepts the concatenation
   of any nonzero number of strings accepted by the parser [p]. *)

(* The parser [p] must be non-nullable, that is, must not accept the empty
   string. This ensures that the mutual recursion in the definitions of [star]
   and [plus] is well-founded: before a recursive call takes place, at list
   one input token is consumed. *)

let rec star (p : 'a parser) : 'a list parser =
  (* TO DO: Define this function. *)
  raise TODO

and plus (p : 'a parser) : 'a list parser =
  (* TO DO: Define this function. *)
  raise TODO

(* The parsers [number_lax] and [number] both recognize a nonempty sequence of
   digits and return its meaning as an integer. Whereas [number_lax] accepts a
   sequence of arbitrary (nonzero) length, [number] has longest-match semantics:
   it will recognize only a sequence of maximum length (thus returns at most one
   result). *)

let decode_digits (digits : int list) : int =
  (* TO DO: Define this function. *)
  raise TODO

let number_lax : int parser =
  (* TO DO: Define this parser. *)
  fail

let number : int parser =
  (* TO DO: Define this parser. *)
  fail

(* Support for iterated applications of left-associative operators. *)

type 'a op =
  'a -> 'a -> 'a

(* The parser [additive_op] recognizes one of the characters '+' or '-'
   and interprets it as the corresponding operation on integers. *)

let additive_op : int op parser =
  (* TO DO: Define this parser. *)
  fail

(* The parser [multiplicative_op] recognizes one of the characters '*' or '/'
   and interprets it as the corresponding operation on integers. *)

let multiplicative_op : int op parser =
  (* TO DO: Define this parser. *)
  fail

(* The parser [chainl1 p op] recognizes the language [p (op p)*]. A string in
   this language is interpreted as a left-associative chain of applications of
   operators to values. *)

let chainl1 (p : 'a parser) (op : 'a op parser) : 'a parser =
  (* TO DO: Define this function. *)
  raise TODO

(* A simple grammar of additions and subtractions of constants. *)

(* A sum is a nonempty list of numbers, separated by additive operators, which
   are considered left-associative. *)

let sum : int parser =
  (* TO DO: Define this parser. *)
  fail

(* The higher-order function [fix] allows defining a recursive parser,
   that is, a parser whose definition refers to itself. Left recursion
   is forbidden: that is, a recursive call is permitted only after a
   nonempty segment of the input has been consumed. *)

let fix (pp : 'a parser -> 'a parser) : 'a parser =
  let rec p cursor = pp p cursor in
  p

(* A simple grammar of arithmetic expressions. *)

(* A term is a nonempty list of factors, separated by additive operators,
   which are left-associative. *)

(* A factor is a nonempty list of atoms, separated by multiplicative
   operators, which are left-associative. *)

(* An atom is either a number or a term surrounded with parentheses. *)

let rec term : int parser =
  (* TO DO: Define this parser. *)
  fail
