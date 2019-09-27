(* -------------------------------------------------------------------------- *)

(* Building an alphabet out of a piece of text. *)

let build_alphabet (text : text) : alphabet =
  let table = Hashtbl.create 256 in
  String.iter (fun symbol ->
    let freq =
      try
        Hashtbl.find table symbol
      with Not_found ->
        0
    in
    Hashtbl.replace table symbol (freq + 1)
  ) text;
  table

(* -------------------------------------------------------------------------- *)

(* Building encoding and decoding dictionaries out of an alphabet. *)

module Q =
  BinomialQueue(struct
    (* The priority queue contains pairs of a tree and a frequency. *)
    type t =
      tree * int
    (* Elements are compared based on their frequency. In other words,
       drawing an element out of the queue yields an element with least
       frequency. *)
    let compare (_, freq1) (_, freq2) =
      freq1 - freq2
  end)

let build_tree (alphabet : alphabet) : tree =
  Leaf 'a'
