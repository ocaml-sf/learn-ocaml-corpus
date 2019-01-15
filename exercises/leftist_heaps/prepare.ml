exception TODO

(* -------------------------------------------------------------------------- *)

(* Definitions that the student relies upon. *)

type priority =
  int

type rank =
  int

(* We fix the type of elements, so that the student does not have to
   write a functor. We make it an algebraic data type, so the student
   does not run a risk of confusing an element with (say) a rank or a
   priority. We do not make it an abstract type, because the student
   may wish to evaluate the counter-examples that we display when we
   find a problem. *)

type element =
  | Red
  | Yellow
  | Green

(* We instrument [priority] so as to count how many calls to it are
   made. This allows to measure the runtime complexity of [union]. *)

let priority = function
  | Red -> 0
  | Yellow -> 1
  | Green -> 2

let calls_to_priority =
  ref 0

let priority x =
  incr calls_to_priority;
  priority x

type heap =
  | E
  | T of rank * element * heap * heap
