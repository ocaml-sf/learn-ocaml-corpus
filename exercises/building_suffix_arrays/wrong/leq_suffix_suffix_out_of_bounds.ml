(* [is_sorted (<=) a] determines whether the array [a] is sorted with respect
   to the preorder [(<=)]. *)

let is_sorted ((<=) : 'a -> 'a -> bool) (a : 'a array) : bool =
  let n = length a in
  let rec is_sorted_from i =
    i >= n-1 || a.(i) <= a.(i+1) && is_sorted_from (i+1)
  in
  is_sorted_from 0

(* [is_permutation p] tests whether the array [p] represents a valid
   permutation of the semi-open interval [0,n), where [n] is [length p]. *)

let is_permutation (p : int array) : bool =
  let exception Invalid in
  let n = length p in
  let hit = make n false in
  try
    for i = 0 to n-1 do
      if 0 <= p.(i) && p.(i) < n then
        if hit.(p.(i)) then
          (* Not injective. *)
          raise Invalid
        else
          hit.(p.(i)) <- true
      else
        (* Out of range. *)
        raise Invalid
    done;
    true
  with Invalid ->
    false

(* [leq_suffix_suffix s i j] is equivalent to [suffix s i <= suffix s j]. *)

let leq_suffix_suffix s i j =
  let n = String.length s in
  let rec loop i j =
    (* i = n || j < n && wrong *) (
         s.[i] < s.[j]
      || s.[i] = s.[j] && loop (i+1) (j+1)
    )
  in
  i = j || loop i j
