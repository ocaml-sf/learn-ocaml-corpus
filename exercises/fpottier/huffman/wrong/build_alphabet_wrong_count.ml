(* -------------------------------------------------------------------------- *)

(* Building an alphabet out of a piece of text. *)

let build_alphabet (text : text) : alphabet =
  let table = Hashtbl.create 256 in
  String.iter (fun symbol ->
    Hashtbl.add table symbol 1 (* wrong: frequency 1 *)
  ) text;
  table
