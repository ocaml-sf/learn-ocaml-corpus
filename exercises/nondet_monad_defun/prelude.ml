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

(* A nondeterministic computation is a data structure, described by the
   following generalized algebraic data type. *)

(* A nondeterministic computation can also be interpreted as a process
   that produces a sequence of results. This interpretation is performed
   by the function [compute]. *)

type 'a m =
| MDelay:               (unit -> 'a m) -> 'a m
| MReturn:                          'a -> 'a m
| MBind:           'a m * ('a -> 'b m) -> 'b m
| MFail:                                  'a m
| MChoose:                 'a m * 'a m -> 'a m
| MReflect: ('a * 'a m) option failure -> 'a m

(* A failure continuation is a data structure, described by the following
   generalized algebraic data type. *)

(* A failure continuation can also be interpreted as a function that takes
   no argument and returns a final answer. This interpretation is performed
   by the function [apply_failure]. *)

(* An [FChoose] entry is known in the logic programming literature as a
   choice point. Each [FChoose] entry contains a pointer [f] to another
   failure continuation, so a failure continuation can be viewed as a
   stack of choice points, represented in memory as a linked list. One
   of [FSols] and [FSplit] serves to indicate the bottom of the stack. *)

and 'answer failure =
| FChoose:
    (* m2: *) 'a m *
    (* s : *) ('a, 'answer) success *
    (* f : *) 'answer failure ->
              'answer failure
| FSols:
    _ Seq.node failure
| FSplit:
    _ option failure

(* A success continuation is a data structure, described by the following
   generalized algebraic data type. *)

(* A success continuation can also be interpreted as a function that takes
   one argument (a result) and returns a final answer. This interpretation
   is performed by the function [apply_success]. *)

(* Each [SBind] entry contains a pointer [s] to another success continuation,
   so a success continuation can be viewed as a stack, represented in memory
   as a linked list. One of [SSols] and [SSplit] serves to indicate the bottom
   of the stack. *)

and ('a, 'answer) success =
| SBind:
    (* m2: *) ('a -> 'b m) *
    (* s : *) ('b, 'answer) success ->
              ('a, 'answer) success
| SSols:
    ('a, 'a Seq.node) success
| SSplit:
    ('a, ('a * 'a m) option) success

(* Implementing this constructor function is immediate. *)

let delay (m : unit -> 'a m) : 'a m =
  MDelay m

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
