(* The module [Seq] is standard as of OCaml 4.07. *)

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

end

(* An implementation of the nondeterminism monad. We use the continuation-
   based implementation. We instrument it with counters so as to be able
   to measure complexity. We distinguish build-time and execution-time
   counters. *)

module BuildCount = struct
  let choose = ref 0
  let reset() =
    choose := 0
end

module ExecCount = struct
  let choose = ref 0
  let reset() =
    choose := 0
end

let reset () =
  BuildCount.reset();
  ExecCount.reset()

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

(* The nondeterminism monad. *)

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
  incr BuildCount.choose;
  let compute s f =
    incr ExecCount.choose;
    m1.compute s (fun () -> m2.compute s f)
  in
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

let once (m : 'a m) : 'a option m =
  delay (fun () ->
    match msplit m () with
    | None ->
        return None
    | Some (x, _) ->
        return (Some x)
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

(* The following auxiliary functions are used during the automatic testing of
   [unfold]. Here, the state [s] is a nonnegative integer; a state [s] is
   considered final when it is zero.

   The computation [decrement s] signals an error if [s] is zero; indeed, a
   step function should never be applied to a final state. If [s] is nonzero,
   then [decrement s] builds a computation with just one outcome, which is to
   decrement [s] by one. By convention, this (unique) choice is numbered 1.

   The computation [decrease s] builds a computation which represents all
   possible ways of strictly decreasing [s], under the constraint that [s]
   must remain nonnegative. By convention, the choice of decreasing [s] to
   [s'] is numbered [s']. *)

let is_zero (s : int) =
  s = 0

let decrement (s : int) : (int * int) m =
  if s = 0 then
    (* A step function should never be applied to a final state. *)
    failwith "final"
  else
    return (1, s - 1)

let rec decrease (s : int) : (int * int) m =
  delay (fun () ->
    if s = 0 then
      (* We must use [fail] here, as this is the base case of the
         recursive function. This case is not dead code. *)
      fail
    else
      (* Either we decrease [s] by one to [s - 1],
         or we decrease it by more than one, which
         is the same as decreasing [s - 1]. *)
      choose
        (return (s - 1, s - 1))
        (decrease (s - 1))
  )

(* A partial placement of queens on the board is an injective function
   of the interval [0, k) into the interval [0, n). We represent it as
   a list of length [k]. *)

type placement =
  int list
