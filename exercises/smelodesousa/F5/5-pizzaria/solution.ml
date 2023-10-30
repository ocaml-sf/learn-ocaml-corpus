(* let score (n : int) (pizza : int list)  =
   let best_sum = ref (List.hd pizza) in
   let current_sum = ref 0 in
   let score_aux x =
     current_sum := max x (!current_sum + x);
     best_sum := max !best_sum !current_sum in
   let () = List.iter score_aux pizza in
   !best_sum *)

(* Another possible solution *)
let score (n : int) (pizza : int list) : int =
  let rec max_sum_aux (pizza : int list) (max_ending_here : int)
      (max_so_far : int) : int =
    match pizza with
    | [] -> max_so_far
    | x :: xs ->
        max_sum_aux xs
          (max 0 (max_ending_here + x))
          (max max_so_far (max_ending_here + x))
  in
  max_sum_aux pizza 0 (-101)
(* minimum value is -100 *)
