(* -------------------------------------------------------------------------- *)

(* Building an alphabet out of a piece of text. *)

let build_alphabet (text : text) : alphabet =
  (* TO DO: Define this function. *)
  raise TODO

(* -------------------------------------------------------------------------- *)

(* Building encoding and decoding dictionaries out of an alphabet. *)

(* TO DO: Define a module [Q] that provides priority queues whose
   elements are pairs of a tree and an integer frequency. Drawing
   an element of the queue must yield an element whose frequency
   is minimum. *)

let build_tree (alphabet : alphabet) : tree =
  (* Assumption: the alphabet has at least two symbols. *)
  assert (Hashtbl.length alphabet >= 2);
  (* TO DO: Define this function. Use the module [Q] above. *)
  raise TODO

let build_encoding_dictionary (tree : tree) : encoding_dictionary =
  (* TO DO: Define this function. *)
  raise TODO

let build_dictionaries alphabet : encoding_dictionary * decoding_dictionary =
  let tree = build_tree alphabet in
  let encoding_dictionary = build_encoding_dictionary tree in
  let decoding_dictionary = tree in
  encoding_dictionary, decoding_dictionary

(* -------------------------------------------------------------------------- *)

(* Encoding input data. *)

let rec find (path : data) (i : int) (t : tree) : char * int =
  assert (0 <= i && i <= String.length path);
  (* TO DO: Define this function. *)
  raise TODO

let encode_char (dictionary : encoding_dictionary) (c : char) : data =
  try
    Hashtbl.find dictionary c
  with Not_found ->
    assert false (* unknown character *)

let encode (dictionary : encoding_dictionary) (text : text) : data =
  let buffer = Buffer.create 1024 in
  String.iter (fun c ->
    Buffer.add_string buffer (encode_char dictionary c)
  ) text;
  Buffer.contents buffer

(* -------------------------------------------------------------------------- *)

(* Decoding compressed data. *)

let decode (tree : tree) (text : data) (i : int) : text =
  let buffer = Buffer.create 1024 in
  let rec loop i =
    if i = String.length text then
      (* We have reached the end of the text. We are done. *)
      Buffer.contents buffer
    else begin
      (* Decode one more character, and continue. *)
      let c, i = find text i tree in
      Buffer.add_char buffer c;
      loop i
    end
  in
  loop i

(* -------------------------------------------------------------------------- *)

(* Serializing a tree: encoding it as a string. *)

let write (tree : tree) : data =
  let buffer = Buffer.create 1024 in
  let rec write (tree : tree) : unit =
    (* TO DO: Define this auxiliary function. *)
    raise TODO
  in
  write tree;
  Buffer.contents buffer

(* Deserializing a tree: reading its representation as a string and
   transforming it back to a tree. *)

let read (s : data) : tree * int =
  let i = ref 0 in
  let get() =
    assert (!i < String.length s);
    let c = s.[!i] in
    incr i;
    c
  in
  let rec read () : tree =
    (* TO DO: Define this auxiliary function. *)
    raise TODO
  in
  let tree = read() in
  tree, !i

(* -------------------------------------------------------------------------- *)

(* Compressing and decompressing an input text. *)

let compress (text : text) : data =
  (* TO DO: Define this function. *)
  raise TODO

let decompress (data : data) : text =
  (* TO DO: Define this function. *)
  raise TODO
