(* A histogram maps characters to integer frequencies. *)

type histogram =
  (char, int) Hashtbl.t

(* Building a histogram out of a string. *)

let histogram (s : string) : histogram =
  let table = Hashtbl.create 256 in
  String.iter (fun symbol ->
    Hashtbl.replace table symbol 1 (* wrong *)
  ) s;
  table

(* Converting a histogram to a list. *)

let elements (h : histogram) : (char * int) list =
  let elements = ref [] in
  Hashtbl.iter (fun symbol freq ->
    elements := (symbol, freq) :: !elements
  ) h;
  !elements

(* Converting a histogram to a sorted list. *)

let elements (h : histogram) : (char * int) list =
  let cmp (c1, _) (c2, _) = compare c1 c2 in
  List.sort cmp (elements h)

(* Comparing two histograms. *)

let eq_histogram (h1 : histogram) (h2 : histogram) : bool =
  elements h1 = elements h2

let anagrams (s1 : string) (s2 : string) : bool =
  (* Two strings are anagrams of one another if and only if they have
     the same histogram. As a fast path, we can first check if the two
     strings have the same length; if they do not, they are definitely
     not anagrams. *)
  String.length s1 = String.length s2 &&
  eq_histogram (histogram s1) (histogram s2)
