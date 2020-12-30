type 'a t = {
  (* A physical array. *)
  mutable table: 'a array;
}

let default_size =
  1600 (* must be non-zero *)

let make x = {
  table = Array.make default_size x;
}

let get a i =
  a.table.(i)

let set a i x =
  a.table.(i) <- x
