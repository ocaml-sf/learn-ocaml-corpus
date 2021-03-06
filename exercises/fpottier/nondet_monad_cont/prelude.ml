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

(* The effect of executing the computation [tick m] is to first increment the
   global counter [work], then execute the computation [m]. *)

(* The grading code uses these operations in order to check that computations
   are executed on demand, that is, as late as possible. *)

let work =
  ref 0

let reset() =
  work := 0

let tick (m : 'a m) : 'a m =
  delay (fun () ->
    work := !work + 1;
    m
  )

let snapshot (x : 'a) : 'a * int =
  (x, !work)
