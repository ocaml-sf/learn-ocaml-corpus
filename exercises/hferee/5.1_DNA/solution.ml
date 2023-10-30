(* Question 1 *)
let is_base = function
  | 'A' | 'C' | 'G' | 'T' -> true
  | _ -> false

let is_dna (s : string) : bool =
  let rec aux n =
    if n >= String.length s then true
    else is_base (String.get s n) && aux (n + 1)
  in
  aux 0


(* Question 2 *)

let conj = function
  | 'A' -> "T"
  | 'C' -> "G"
  | 'G' -> "C"
  | _ -> "A"

let complement s =
  let rec aux n = if n = String.length s then ""
    else conj (String.get s n) ^ aux (n + 1) in
  aux 0

(* Question 3 *)

let first_stop s =
  let rec aux n =
    if n + 3 > String.length s then String.length s else
      match String.get s n, String.get s (n + 1), String.get s (n + 2) with
      | 'T', 'G', 'A' | 'T', 'A', 'A' | 'T', 'A', 'G' -> n
      | _ -> aux (n + 1)
  in aux 0

