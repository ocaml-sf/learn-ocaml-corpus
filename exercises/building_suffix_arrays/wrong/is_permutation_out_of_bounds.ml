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
      if hit.(p.(i)) then
        (* Not injective. *)
        raise Invalid
      else
        hit.(p.(i)) <- true
    done;
    true
  with Invalid ->
    false
