(* A type of binary trees. *)

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

(* The module [Seq] is standard as of OCaml 4.07. *)

module Seq = struct

  type 'a t = unit -> 'a node

  and +'a node =
  | Nil
  | Cons of 'a * 'a t

  let nil () = Nil

  let cons x xs () = Cons (x, xs)

  (* The sequence [trap] always raises the exception [Trap] when
     it is queried. It is used by the automatic grader and can
     appear in some messages produced by the grader. *)

  exception Trap

  let trap () = raise Trap

end
