(* [is_sorted (<=) a] determines whether the array [a] is sorted with respect
   to the preorder [(<=)]. *)

let is_sorted ((<=) : 'a -> 'a -> bool) (a : 'a array) : bool =
  let exception Invalid in
  let n = length a in
  try
    for i = 0 to n-2 do
      if a.(i) > a.(i+1) then (* wrong! uses (>) *)
        raise Invalid
    done;
    true
  with Invalid ->
    false
