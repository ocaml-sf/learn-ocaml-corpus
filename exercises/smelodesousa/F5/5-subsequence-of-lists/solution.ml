let subseq w1 w2 =
  let rec aux l1 l2 =
    match (l1, l2) with
    | h1 :: t1, h2 :: t2 -> if h1 = h2 then aux t1 t2 else aux l1 t2
    | _ :: _, [] -> false
    | [], _ -> true
  in
  aux w1 w2

(* Original version:
   let subseq l1 l2 =
     let rec iter_list l1 l2 =
       try
         match l2 with
         | []   -> l1
         | h::t ->
             iter_list (if h=(List.hd l1) then (List.tl l1) else l1) t
       with _ -> l1 in
     [] = (iter_list l1 l2) *)
