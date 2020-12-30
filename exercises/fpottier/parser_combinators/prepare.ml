exception TODO

(* -------------------------------------------------------------------------- *)

(* Sequences. *)

module Seq = struct

  type 'a t = unit -> 'a node

  and +'a node =
  | Nil
  | Cons of 'a * 'a t

  let nil =
    fun () -> Nil

  let cons x xs =
    fun () -> Cons (x, xs)

  let singleton x =
    cons x nil

  let rec map (f : 'a -> 'b) (xs : 'a t) : 'b t =
    fun () ->
      match xs() with
      | Nil ->
          Nil
      | Cons (x, xs) ->
          Cons (f x, map f xs)

  let rec concat (xs : 'a t) (ys : 'a t) : 'a t =
    fun () ->
      match xs() with
      | Nil ->
          ys()
      | Cons (x, xs) ->
          Cons (x, concat xs ys)

  let rec flatten (xss : 'a t t) : 'a t =
    fun () ->
      match xss() with
      | Nil ->
          Nil
      | Cons (xs, xss) ->
          concat xs (flatten xss) ()

  let rec take n (xs : 'a t) : 'a t =
    if n = 0 then
      nil
    else
      fun () ->
        match xs() with
        | Nil ->
            Nil
        | Cons (x, xs) ->
            Cons (x, take (n-1) xs)

  let head (xs : 'a t) : 'a option =
    match xs() with
    | Nil ->
        None
    | Cons (x, _) ->
        Some x

  let rec of_list (xs : 'a list) : 'a t =
    fun () ->
      match xs with
      | [] ->
          Nil
      | x :: xs ->
          Cons (x, of_list xs)

  (* [of_string] turns an OCaml string into a sequence of characters. *)

  let rec of_substring s i j : char t =
    fun () ->
      if i = j then
        Nil
      else
        Cons (s.[i], of_substring s (i+1) j)

  let of_string s : char t =
    of_substring s 0 (String.length s)

  (* A word of warning: [to_list] does not terminate if it is applied
     to an infinite sequence. Furthermore, this version of [to_list]
     is not tail-recursive and could exhaust the stack space if it was
     applied to a long sequence. *)

  let rec to_list (xs : 'a t) : 'a list =
    match xs() with
    | Nil ->
        []
    | Cons (x, xs) ->
        x :: to_list xs

  (* A word of warning: [length] does not terminate if it is applied
     to an infinite sequence. *)

  let rec length accu (xs : 'a t) : int =
    match xs() with
    | Nil ->
        accu
    | Cons (_, xs) ->
        length (accu + 1) xs

  let length xs =
    length 0 xs

  (* A word of warning: [equal xs xs] does not terminate if [xs] is an
     infinite sequence. *)

  let rec equal (eq : 'a -> 'a -> bool) (xs : 'a t) (ys : 'a t) : bool =
    match xs(), ys() with
    | Nil, Nil ->
        true
    | Cons (x, xs), Cons (y, ys) ->
        eq x y && equal eq xs ys
    | _, _ ->
        false

end

(* -------------------------------------------------------------------------- *)

(* An implementation of the nondeterminism monad. *)

module NonDet = struct

  (* A failure continuation takes no argument and returns a final answer. *)

  type 'answer failure =
    unit -> 'answer

  (* A success continuation takes one argument (a result) and returns a final
     answer. *)

  type ('a, 'answer) success =
    'a -> 'answer failure -> 'answer

  (* A nondeterministic computation produces a sequence of results.
     It is represented (in this implementation) as a function [compute]
     which accepts a success continuation [s] and a failure continuation [f].
     It is polymorphic in the answer type of these continuations. *)

  (* The OCaml annotation [@@unboxed] improves efficiency, but otherwise
     has no impact. *)

  type 'a m = {
    compute: 'answer . ('a, 'answer) success -> 'answer failure -> 'answer
  } [@@unboxed]

  (* In the implementation of [delay], the function call [m()] takes place
     only when [compute] is invoked -- not right now. *)

  let delay (m : unit -> 'a m) : 'a m =
    let compute s f = (m()).compute s f in
    { compute }

  let return (x : 'a) : 'a m =
    let compute s f = s x f in
    { compute }
    (* We are able to produce the result [x]. To do so, we invoke the
       success continuation [s] with [x]. Because we are unable to produce
       more results, if the outside world requests more results from us,
       we must fail; to indicate this, we directly pass our own failure
       continuation [f] to [s]. This is a tail call in CPS style. *)

  let (>>=) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
    let compute s f = m1.compute (fun x f' -> (m2 x).compute s f') f in
    { compute }
    (* In order to produce anything, we must first execute [m1]. If it
       fails, we fail as well; we indicate this by passing [f] as the
       failure continuation. Every time [m1] produces an element [x],
       we execute the computation [m2 x]. Every result produced by it
       is passed directly to our success continuation [s]. If and when
       [m2 x] fails, we must request the next [x], which is done by
       calling [f'], the failure continuation that came with [x]. *)

  let fail : 'a m =
    let compute s f = f() in
    { compute }
    (* We are unable to produce any result, so we immediately invoke
       the failure continuation [f]. *)

  let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
    let compute s f = m1.compute s (fun () -> m2.compute s f) in
    { compute }
    (* We first execute [m1] and let it pass each of its results directly
       to [s]. Once it is done, it fails and calls the failure continuation
       that we have built. There, we execute [m2] and let it pass each of
       its results directly to [s]. Once it is done, it fails; we provide
       our own failure continuation [f] to it, so that this failure propagates
       upwards without passing through us. *)

  let sols (m : 'a m) : 'a Seq.t =
    fun () ->
      m.compute
        (fun x f -> Seq.Cons (x, f))
        (fun () -> Seq.Nil)

  let reflect (o : unit -> ('a * 'a m) option) : 'a m =
    delay (fun () ->
      match o() with
      | None ->
          fail
      | Some (x, m) ->
          choose (return x) m
    )

  let msplit (m : 'a m) : unit -> ('a * 'a m) option =
    fun () ->
      m.compute
        (fun x f -> Some (x, reflect f))
        (fun  () -> None)

  let at_most_once (m : 'a m) : 'a m =
    delay (fun () ->
      match msplit m () with
      | None ->
          fail
      | Some (x, _) ->
          return x
    )

  let rec interleave (m1 : 'a m) (m2 : 'a m) : 'a m =
    delay (fun () ->
      match msplit m1 () with
      | None ->
          m2
      | Some (x1, m1) ->
          choose (return x1) (interleave m2 m1)
    )

  let rec (>>-) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
    delay (fun () ->
      match msplit m1 () with
      | None ->
          fail
      | Some (x1, m1) ->
          interleave (m2 x1) (m1 >>- m2)
    )

end

(* -------------------------------------------------------------------------- *)

(* Type definitions. *)

(* In order to avoid verbosity in the types, we fix the type of tokens to be
   [char]. However, we could parameterize everything over a type ['c], and
   everything would work; we need no operations at all on tokens, not even an
   equality test, except in the function [char]. A token could be a pair of a
   character and a position, for instance. *)

type token =
  char

(* We need only one operation on cursors, namely reading, which takes a cursor
   and returns either nothing or a pair [(c, cursor)], where [c] is an input
   character (or token) and [cursor] is another cursor. Thus, we can take a
   cursor to be a (finite or infinite) sequence of tokens. *)

(* We could require a cursor to be an infinite sequence of tokens. That would
   force the client to pad a finite sequence with an infinite repetition of
   some [EOF] token. That would work fine, but would prevent us from using the
   standard type [_ Seq.t]. *)

type cursor =
  token Seq.t

(* A parser is a nondeterministic computation that maintains a state of type
   [cursor]. *)

(* This is just an application of the state monad transformer to the
   nondeterminism monad. It is still preferable to inline it, for clarity. *)

type 'a parser =
  cursor -> ('a * cursor) NonDet.m

(* To run a parser, one applies it to the input (a sequence of characters).
   This yields a computation in the [NonDet] monad. Running this computation,
   by applying the function [NonDet.sols] to it, yields a sequence of pairs of
   a result and a final cursor. As we are not interested in the final cursors,
   we drop them, yielding a sequence of results. *)

(* If the parser [p] is of the form [_ << eof], then it can succeed only at
   the end of the input stream, so every final cursor must in fact be an empty
   sequence of characters. No information is lost by dropping it. *)

let run (p : 'a parser) (input : string) : 'a Seq.t =
  input
  |> Seq.of_string
  |> p
  |> NonDet.sols
  |> Seq.map fst

(* -------------------------------------------------------------------------- *)

(* We want to avoid division-by-zero exceptions in this exercise. (They could
   occur during random testing if we are unlucky, as we evaluate arbitrary
   arithmetic expressions in Question 4.) To do so without fuss, we redefine
   division to never raise an exception. *)

let (/) x y =
  if y = 0 then x else x / y
