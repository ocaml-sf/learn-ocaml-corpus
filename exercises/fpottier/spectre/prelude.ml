(* A type of trees where only the leaves carry data. *)

type 'a tree =
  | Leaf of 'a
  | Fork of 'a tree * 'a tree

(* A depth is a natural integer. *)

type depth =
  int

(* A spectre is a list of pairs of an element and its depth. *)

type 'a spectre =
  ('a * depth) list

(* A facility for reading and consuming the elements of a list. *)

type position =
  int

type 'a input =
  {
    peek: unit -> 'a option;
    consume: unit -> unit;
    current: unit -> position;
  }

(* The following exceptions are intended to be raised by [build]. *)

exception InputIsTooShort of position
exception InputIsTooLong of position
exception InputIsIllFormed of position
