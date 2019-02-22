exception TODO

(* Restrict the polymorphic equality and comparison operators. *)

module Backup = struct
  let (=) = (=)
  let (<=) = (<=)
end

let (=) : int -> int -> bool =
  (=)

let (<=) : int -> int -> bool =
  (<=)
