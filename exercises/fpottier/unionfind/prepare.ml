exception TODO

(* We instrument reads on references. This allows to measure the runtime
   complexity of [union]. We redefine the type ['a ref] as an abstract
   type so as to prevent the user from circuvemting our mechanism using
   pattern matching. *)

module OriginalRefs__ = struct
  type nonrec 'a ref = 'a ref
  let ref = ref
  let (:=) = (:=)
  let (!) = (!)
end

module R : sig
  type 'a ref
  val ref: 'a -> 'a ref
  val (!): 'a ref -> 'a
  val (!!): 'a ref -> 'a
  val (:=): 'a ref -> 'a -> unit
  val cost: ('a -> 'b) -> 'a -> int * 'b
end = struct
  let (!!) = (!) (* !! is a zero-cost read; not for use by the student! *)
  let (!!), ref, (:=) = (!), ref, (:=)
  type nonrec 'a ref = 'a ref
  let c = ref 0
  let reset() = c := 0
  let reads() = !c
  let cost f x =
    reset();
    let y = f x in
    reads(), y
  let (!) r = incr c; !r
end

open R

(* The following type definitions should ideally be public, but we cannot
   place them in prelude.ml because prepare depends on prelude, not the
   other way around. *)

type elem =
  content ref

and content =
| Link of elem (* parent *)
| Root of rank (* rank *)

and rank =
  int
