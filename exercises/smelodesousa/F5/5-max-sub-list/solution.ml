(* 1 *)
let max_kadane l =
  let rec aux sum ret = function
    | h :: t ->
        let cur_max = max (sum + h) h in
        aux cur_max (max cur_max ret) t
    | [] -> ret
  in
  match l with
  | [] -> 0
  | h :: t ->
      let result = aux h h t in
      if result < 0 then 0 else result

(* 2 *)
let kadane l =
  let rec aux sum maxVal ret l =
    if sum = maxVal then ret
    else
      match l with
      | h :: t ->
          if sum + h < h then aux h maxVal [ h ] t
          else aux (sum + h) maxVal (h :: ret) t
      | [] -> ret
  in
  match l with
  | h :: t ->
      let max = max_kadane l in
      if max = 0 then [] else List.rev (aux h max [ h ] t)
  | [] -> []

(* Original version:
   (* 1 *)
   let rec max_liste l =
     match l with
     | [] -> assert false
     | [x] -> x
     | x :: s -> max x (max_liste s)

   let max_kadane l =
     let rec kad_rec l m =
     match l, m with
     | [], _ -> max_liste m
     | x :: r, y :: _ ->
         let z = max x (x+y) in
         kad_rec r (z::m)
     | _ -> assert false
     in
     kad_rec l [0]

   (* 2 *)
   let rec loop sum l seq maxsum maxseq =
     match l with
     | [] -> List.rev maxseq
     | x::xs ->
         let sum = sum + x and seq = x :: seq in
           if sum < 0 then
             loop 0 xs [] maxsum maxseq
           else if sum > maxsum then
             loop sum xs seq sum seq
           else
             loop sum xs seq maxsum maxseq


   let kadane l =
       loop 0 l [] 0 [] *)
