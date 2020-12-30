(* -------------------------------------------------------------------------- *)

(* Building an alphabet out of a piece of text. *)

let build_alphabet (text : text) : alphabet =
  let table = Hashtbl.create 256 in
  String.iter (fun symbol ->
    let freq =
      Hashtbl.find table symbol
      (* wrong: forget to catch Not_found *)
    in
    Hashtbl.replace table symbol (freq + 1)
  ) text;
  table
