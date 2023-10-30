let rec euclid a b =
  match (a, b) with
  | invalid_a, invalid_b when a < 0 || b < 0 ->
      raise (Invalid_argument "euclid")
  | _, 0 -> a
  | _ -> euclid b (a mod b)

(* Original version:
   let rec euclid a b =
     if a < 0 || b < 0 then raise (Invalid_argument "euclid");
     if b = 0 then a
     else euclid b (a mod b) *)
