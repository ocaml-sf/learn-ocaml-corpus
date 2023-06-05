(* let rec comb n k =
     if k < 0 || n < 0 then 0
     else if k = 0 || k = n then 1
     else (comb (n-1) (k-1)) + (comb (n-1) k)

   let narayana n k =
     ((comb n k) * (comb n (k-1))) / n

   let mountains n k =
     if k > n || k < 1 then None
     else Some (narayana n k) *)

(* Another possible solution *)
let mountains n k =
  if k < 1 || k > n then None
  else
    let rec aux_fun n k res =
      if k < 0 || n < 0 then res
      else if k = 0 || k = n then res + 1
      else aux_fun (n - 1) (k - 1) (aux_fun (n - 1) k res)
    in
    Some (aux_fun n k 0 * aux_fun n (k - 1) 0 / n)
