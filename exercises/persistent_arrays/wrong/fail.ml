let make (n : int) (x : 'a) : 'a parray =
  failwith "FUBAR" (* wrong *)

let set (base : 'a parray) (i : int) (value : 'a) =
  ref (Apply { base; i; value })

let get (a : 'a parray) (i : int) : 'a =
  failwith "SNAFU" (* wrong *)
