(* Addition of requirements. *)

let (++) (x : req) (y : req) : req =
  (* TO DO: Define this function. *)
  raise TODO

(* Comparison of requirements. *)

let (<==) (x : req) (y : req) : bool =
  (* TO DO: Define this function. *)
  raise TODO

(* Determining the space requirement of a document. *)

(* This function is expected to run in constant time. *)

let rec requirement (doc : doc) : req =
  (* TO DO: Define this function. *)
  raise TODO

(* Smart constructors. *)

let empty : doc =
  Empty

let hardline : doc =
  HardLine

let char (c : char) : doc =
  (* TO DO: Define this function. *)
  raise TODO

let ifflat (doc1 : doc) (doc2 : doc) : doc =
  (* TO DO: Define this function. *)
  raise TODO

let (^^) (doc1 : doc) (doc2 : doc) : doc =
  (* TO DO: Define this function. *)
  raise TODO

let nest (i : int) (doc : doc) : doc =
  (* TO DO: Define this function. *)
  raise TODO

let group (doc : doc) : doc =
  (* TO DO: Define this function. *)
  raise TODO

let rec pretty state (indent : int) (flatten : bool) doc =
  (* TO DO: Define this function. *)
  raise TODO

let pretty width doc =
  (* TO DO: Define this function. *)
  raise TODO

