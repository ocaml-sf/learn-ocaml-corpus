let sum lista = 
  List.fold_left (+) 0 lista

let rec count_even l = 
  match l with
  | [] -> 0
  | h::t -> if (h mod 2) = 0 then 1 + (count_even t) else (count_even t)

let palindrome l =
  l = List.rev l

let uppercase l = 
  List.map (fun x -> if (x >= 'a' && x <= 'z') then char_of_int((int_of_char x) - 32) else x) l

let rec is_sorted (l : int list) f =
  match l with 
  | [] -> true
  | h1::[] -> true
  | h1::h2::t -> if ((f h1 h2) = (-1) || (f h1 h2) = 0) then (true && is_sorted (h2::t) f) else false

let rec remove_duplicate_sorted l = 
  match l with
  | [] -> []
  | h::t -> h::(remove_duplicate_sorted (List.filter (fun x -> x<>h) t))

let rec remove_duplicate l = 
  match (List.sort compare l) with
  | [] -> []
  | h::t -> h::(remove_duplicate (List.filter (fun x -> x<>h) t))