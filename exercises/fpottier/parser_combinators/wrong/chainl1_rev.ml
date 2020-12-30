(* The type [parser] satisfies the monad API: [delay], [return], [>>=]. *)

(* [delay p] behaves like [p()], except the construction of the monadic
   computation is delayed until the moment when this computation is run. *)

let delay (p : unit -> 'a parser) : 'a parser =
  fun cursor ->
    NonDet.delay (fun () -> p () cursor)

(* [return a] always succeeds, consumes no input, and returns [a]. *)

let return (a : 'a) : 'a parser =
  fun cursor ->
    NonDet.return (a, cursor)

(* [p >>= f] is the sequential composition of the parsers [p] and [f]. *)

let (>>=) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  fun cursor ->
    let open NonDet in
    p cursor >>= fun (a, cursor) ->
    f a cursor

(* The type [parser] supports the nondeterminism monad API: [fail], [choose],
   [at_most_once]. *)

(* The parser [fail] accepts the empty language: it always fails. *)

let fail : 'a parser =
  fun cursor ->
    NonDet.fail

(* The parser [choose p q] accepts the union of the languages accepted
   by the parsers [p] and [q]. *)

let choose (p : 'a parser) (q : 'a parser) : 'a parser =
  fun cursor ->
    NonDet.choose (p cursor) (q cursor)

(* The parser [at_most_once p] accepts the same language as [p], but
   accepts each input string in at most one way. *)

let at_most_once (p : 'a parser) : 'a parser =
  fun cursor ->
    NonDet.at_most_once (p cursor)

(* The type [parser] supports the applicative functor API: [map], [<*>],
   [<&>]. *)

(* [map f p] accepts the same language as the parser [p], and applies the
   function [f] to every result produced by [p]. *)

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  p >>= fun a ->
  return (f a)

(* The parser [p <*> q] accepts the concatenation of the languages accepted
   by the parsers [p] and [q]. For each value [f] returned by [p] and for
   each value [x] returned by [q], it returns the result of the application
   [f x]. *)

let (<*>) (p : ('a -> 'b) parser) (q : 'a parser) : 'b parser =
  p >>= fun f ->
  q >>= fun x ->
  return (f x)

(* The parser [p <&> q] accepts the concatenation of the languages accepted
   by the parsers [p] and [q]. For each value [a] returned by [p] and for
   each value [b] returned by [q], it returns the pair [(a, b)]. *)

let (<&>) (p : 'a parser) (q : 'b parser) : ('a * 'b) parser =
  p >>= fun a ->
  q >>= fun b ->
  return (a, b)

(* The parser [p >> q] accepts the concatenation of the languages accepted
   by the parsers [p] and [q]. For each value [a] returned by [p] and for
   each value [b] returned by [q], it returns just [b]. *)

let (>>) (p : 'a parser) (q : 'b parser) : 'b parser =
  p >>= fun _ -> q

(* The parser [p << q] accepts the concatenation of the languages accepted
   by the parsers [p] and [q]. For each value [a] returned by [p] and for
   each value [b] returned by [q], it returns just [a]. *)

let (<<) (p : 'a parser) (q : 'b parser) : 'a parser =
  p >>= fun a ->
  q >> return a

(* The parser combinator [list] is an n-ary version of [<&>]. If [ps] is
   a list of parsers, then the parser [list ps] accepts the concatenation
   of the languages accepted by the parsers in the list [ps]. *)

let cons (x, xs) =
  x :: xs

let rec list (ps : 'a parser list) : 'a list parser =
  match ps with
  | [] ->
      return []
  | p :: ps ->
      map cons (p <&> list ps)

(* The type [parser] supports a number of operations that are specific of the
   parser combinator monad. The most basic two are [any] and [eof]. On top of
   these two, more combinators are defined, including [sat], [char], [string],
   [digit], and so on. *)

(* [any] succeeds if and only if the cursor is currently not at the end of the
   input stream. It consumes and returns the next input token. *)

let any : token parser =
  fun cursor ->
    match cursor() with
    | Seq.Nil ->
        NonDet.fail
    | Seq.Cons (c, cursor) ->
        NonDet.return (c, cursor)

(* [eof] succeeds if and only if the cursor is currently at the end of the
   input stream. It consumes nothing, and returns [()]. It is idempotent:
   [eof >> eof] is the same as [eof]. *)

let eof : unit parser =
  fun cursor ->
    match cursor() with
    | Seq.Nil ->
        NonDet.return ((), cursor)
    | Seq.Cons _ ->
        NonDet.fail

(* The parser [sat p] consumes and returns the next input token if this
   token satisfies the predicate [p]. It fails otherwise. *)

let sat (p : token -> bool) : token parser =
  any >>= fun c ->
  if p c then return c else fail

(* The parser [char c] consumes and returns the next input token if this
   token is the character [c]. It fails otherwise. *)

let char (c : token) : token parser =
  sat ((=) c)

(* The parser [string cs] consumes and returns the list of characters [cs]
   if this exact list is found at the front of the input stream. It fails
   otherwise. *)

let string (cs : token list) : token list parser =
  list (List.map char cs)

(* The parser [digit] consumes the next token if this token is a character
   in the range ['0'..'9']. It returns an integer between 0 and 9. *)

let is_digit c : bool =
  Char.code '0' <= Char.code c && Char.code c <= Char.code '9'

let decode_digit c : int =
  Char.code c - Char.code '0'

let digit: int parser =
  sat is_digit |> map decode_digit

(* Iteration. *)

(* The parser [star p] accepts the concatenation of any number of strings
   accepted by the parser [p]. The parser [plus p] accepts the concatenation
   of a nonzero number of strings accepted by the parser [p]. *)

(* The parser [p] must be non-nullable, that is, must not accept the empty
   string. This ensures that the mutual recursion in the definitions of [star]
   and [plus] is well-founded: before a recursive call takes place, at list
   one input token is consumed. *)

let rec star (p : 'a parser) : 'a list parser =
  choose
    (plus p)
    (return [])

and plus (p : 'a parser) : 'a list parser =
  p >>= fun a ->
  star p >>= fun aas ->
  return (a :: aas)

(* The parser [number] recognizes a nonempty sequence of digits and returns
   their meaning as an integer. *)

let decode_digits (digits : int list) : int =
  List.fold_left (fun accu digit -> 10 * accu + digit) 0 digits

let number_lax : int parser =
  plus digit |> map decode_digits

let number : int parser =
  at_most_once number_lax
    (* This works because, in the definition of [star], we explore
       the longest match first. *)

(* Support for iterated applications of left-associative operators. *)

(* The parser [chainl1 p op] recognizes the language [p (op p)*]. A string in
   this language is interpreted as a left-associative chain of applications of
   operators to values. *)

type 'a op =
  'a -> 'a -> 'a

let chainr1 (p : 'a parser) (op : 'a op parser) : 'a parser =
  star (p <&> op) <&> p
  |> map (fun (ops, p) ->
    List.fold_right (fun (p1, op) p2 -> op p1 p2) ops p
  )

(* wrong: *)
let chainl1 = chainr1

(* The parser [additive_op] recognizes one of the characters '+' or '-'
   and interprets it as the corresponding operation on integers. *)

let additive_op : int op parser =
  choose
    (char '+' >> return ( + ))
    (char '-' >> return ( - ))

(* The parser [multiplicative_op] recognizes one of the characters '*' or '/'
   and interprets it as the corresponding operation on integers. *)

let multiplicative_op : int op parser =
  choose
    (char '*' >> return ( * ))
    (char '/' >> return ( / ))

(* A simple grammar of additions and subtractions of constants. *)

(* A sum is a nonempty list of numbers, separated by additive operators, which
   are considered left-associative. *)

let sum : int parser =
  chainl1 number additive_op

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
  fix (fun term ->
    let atom : int parser =
      choose
        number
        (char '(' >> term << char ')')
    in
    let factor : int parser =
      chainl1 atom multiplicative_op
    in
    let term : int parser =
      chainl1 factor additive_op
    in
    term
  )
