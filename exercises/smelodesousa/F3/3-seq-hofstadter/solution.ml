let rec hhq r s n =
  if s < 2 || r >= s then raise (Failure "hhq")
  else if 1 <= n && n <= s then 1
  else if n > s then
    hhq r s (n - hhq r s (n - r)) + hhq r s (n - hhq r s (n - s))
  else raise (Failure "hhq")

(* Original version:
   let rec hhq r s n =
     begin
       if n <= 0 || r>=s || s<2 then raise(Failure "hhq");
       if n <= s && n >= 1 then 1
       else (hhq r s (n - (hhq r s (n-r)))) + (hhq r s (n - (hhq r s (n-s))))
     end *)
