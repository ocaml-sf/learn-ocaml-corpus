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
  (* Assumption: the alphabet has at least two symbols. *)
  assert (Hashtbl.length alphabet >= 2);
  (* Initialize a priority queue. *)
  let queue : Q.t =
    Hashtbl.fold (fun symbol freq queue ->
      Q.insert (Leaf symbol, freq) queue
    ) alphabet Q.empty
  in
  (* Process the priority queue. *)
  let rec process (queue : Q.t) : tree =
    (* Assumption: [queue] is nonempty. *)
    assert (not (Q.is_empty queue));
    let (tree0, freq0), queue = Q.extract queue in
    (* If the queue is now empty, we are done. *)
    if Q.is_empty queue then
      tree0
    else
      (* Otherwise, extract another tree. *)
      let (tree1, freq1), queue = Q.extract queue in
      (* Construct a new node, compute its cumulated frequency,
         insert it back into the queue, and continue. *)
      let tree = Node (tree0, tree0) in (* wrong: drop [tree1] and duplicate [tree0] *)
      let freq = freq0 + freq1 in
      let queue = Q.insert (tree, freq) queue in
      process queue
  in
  process queue
