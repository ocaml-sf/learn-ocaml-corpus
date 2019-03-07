(* The type of requirements. *)

type req =
  | Infinite
  | Finite of int (* always nonnegative *)

(* The type of documents. *)

type doc =
  | Empty
  | HardLine
  | Char of char (* never '\n' *)
  | Cat of req * doc * doc
  | Nest of int * req * doc
  | Group of req * doc
  | IfFlat of doc (* never [IfFlat _] *) * doc

(* The internal state of the rendering engine. *)

type state =
  {
    (* The line width. *)
    width: int;
    (* The current column. *)
    mutable column: int;
    (* The output buffer. *)
    output: Buffer.t;
  }
