exception TODO

module InfiniteArray : sig
  type 'a t
  val make: 'a -> 'a t
  val get: 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> unit
end = struct

  type 'a t = {
    default: 'a;
    mutable table: 'a array;
  }

  let default_size =
    16 (* must be non-zero *)

  let make x = {
    default = x;
    table = Array.make default_size x;
  }

  let rec new_length length i =
    if i < length then
      length
    else
      new_length (2 * length) i

  let ensure a i =
    assert (0 <= i);
    let table = a.table in
    let length = Array.length table in
    if i >= length then begin
      let table' = Array.make (new_length (2 * length) i) a.default in
      Array.blit table 0 table' 0 length;
      a.table <- table'
    end

  let get a i =
    ensure a i;
    Array.unsafe_get a.table (i)

  let set a i x =
    ensure a i;
    Array.unsafe_set a.table (i) x

end

exception ExpectedGot of int * int

exception ExpectedGotB of bool * bool
exception NotASetElement of int
exception ExpectedNoneGotSome of int
exception ExpectedSomeGotNone
