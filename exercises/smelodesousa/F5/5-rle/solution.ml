let p1 = B

(* -------------------- ENCODER -------------------- *)
let rec countOccr x l = 
  match l with
  [] -> 0
  | h::t -> if h=x then 1 + (countOccr x t) else 0

let rec split n l = 
  if n = 0
  then l
  else match l with [] -> [] | h::t -> split (n-1) t

let rec rle_encode l = 
  match l with 
  [] -> []
  | h::t -> let occur = countOccr h l in let new_list = split occur l in match occur with 1 -> (One h)::(rle_encode new_list) | _ -> (Many (h,occur))::(rle_encode new_list)


(* -------------------- DECODER -------------------- *)
let rec listBuilder x n =
  match n with
  0 -> []
  | _ -> x::(listBuilder x (n-1))

let rec rle_decode l =
  match l with
  [] -> []
  | h::t -> match h with One x -> [x]@(rle_decode t) | Many (x,n) -> (listBuilder x n)@(rle_decode t)