(* BEGIN EXCLUDE
(* A histogram maps characters to integer frequencies. *)

module CharMap =
  Map.Make(Char)

type histogram =
  int CharMap.t

(* Building a histogram out of a string. *)

let histogram (s : string) : histogram =
  let table = ref CharMap.empty in
  String.iter (fun c ->
    let freq = try CharMap.find c !table with Not_found -> 0 in
    table := CharMap.add c (freq + 1) !table
  ) s;
  !table

     END EXCLUDE *)
let anagrams (s1 : string) (s2 : string) : bool =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  (* Two strings are anagrams of one another if and only if they have
     the same histogram. As a fast path, we can first check if the two
     strings have the same length; if they do not, they are definitely
     not anagrams. *)
  String.length s1 = String.length s2 &&
  CharMap.equal (=) (histogram s1) (histogram s2)
     END EXCLUDE *)
