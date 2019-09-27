(* We refer to the input data (which we wish to compress) as "text". *)

type text =
  string

(* A Huffman tree is a binary tree whose leaves carry a character. *)

type tree =
| Leaf of char
| Node of tree * tree

(* As a simple-minded representation of binary data, we use strings
   made up exclusively of the characters '0' and '1'. *)

type data =
  string

(* An alphabet maps characters to integer frequencies. *)

type alphabet =
  (char, int) Hashtbl.t

(* An encoding dictionary maps input characters to binary strings. *)

type encoding_dictionary =
  (char, data) Hashtbl.t

(* A decoding dictionary is a Huffman tree. *)

type decoding_dictionary =
  tree

(* Sorting. *)

let sort : char list -> char list =
  List.sort compare

(* The leaves of a tree. *)

let leaves tree =
  let rec leaves tree accu =
    match tree with
    | Leaf c ->
        c :: accu
    | Node (tree0, tree1) ->
        leaves tree0 (leaves tree1 accu)
  in
  leaves tree []

(* [entries table] produces a list of the key-value pairs found in the hash
   table [table]. The list is sorted by key using OCaml's generic comparison
   function [compare]. *)

let entries table =
  Hashtbl.fold (fun key value entries ->
    (key, value) :: entries
  ) table []
  |> List.sort (fun (key1, _) (key2, _) -> compare key1 key2)

(* [write_char b c] converts the character [c] to a sequence of 8 binary
   characters (each of which is '0' or '1'). This sequence is written to
   the buffer [b]. *)

let write_char (b : Buffer.t) (c : char) =
  let c = ref (Char.code c) in
  for i = 0 to 7 do
    Buffer.add_char b (if !c land 0x80 = 0 then '0' else '1');
    c := !c lsl 1
  done

(* Assuming that the function [next] is a source of characters (i.e., every
   time it is called, it produces the next character, read from some source)
   [read_char next] reads 8 binary characters (each of which is '0' or '1')
   and combines them to produce a single character. It is the inverse of
   [write_char] above. *)

let read_char (next : unit -> char) : char =
  let c = ref 0 in
  let mask = ref 0x80 in
  for i = 0 to 7 do
    if next() = '1' then c := !c lor !mask;
    mask := !mask lsr 1
  done;
  Char.chr !c
