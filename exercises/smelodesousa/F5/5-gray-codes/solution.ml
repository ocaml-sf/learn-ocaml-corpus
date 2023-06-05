(* 1 *)
let gray_list n =
  if n < 0 then raise(Invalid_argument "gray_list");
  let n1 = n + 1 in 
  let rec gray_next_level k l =
    if k<n1 then
      let (first_half,second_half) =
        List.fold_left (fun (acc1,acc2) x ->
            (("0"^x)::acc1, ("1"^x)::acc2 )) ([],[]) l
      in
      gray_next_level (k+1) (List.rev_append first_half second_half)
    else l
  in
  gray_next_level 1 ["0"; "1"];;

(* 2 *)
let dec_to_bin x =
  let rec d2b y lst = match y with 0 -> lst
    | _ -> d2b (y/2) ((y mod 2)::lst)
  in
  d2b x [];;

let with_nth_char m c = 
  String.mapi (fun i b -> if i = m then c else b);;

let replace i = 
  if i = 0 then '0' else '1';;

let replace_opt i = 
  match i with
  | None -> '0'
  | Some a -> if a = 0 then '0' else '1';;

let gray_code n = 
  let binary = dec_to_bin n in
  let size = List.length (binary) in 
  let result = ref (String.make size '0') in
  let rec aux i = 
    if i = 0 then result := (with_nth_char i (replace_opt (List.nth_opt binary i)) !result)
    else result := (with_nth_char (i) (replace ((List.nth binary (i-1)) lxor (List.nth binary (i)))) !result);
    if i >= (size - 1) then if n = 0 then "0" else !result else aux (succ i) in
  aux 0;;

(* 3 *)
let gray b =
  if b < 0 then raise (Invalid_argument "gray");
  b lxor (b lsr 1)

(* 4 *)
let de_gray n =
  if n < 0 then raise (Invalid_argument "de_gray");
  let rec aux p n =
    if n = 0 then p
    else aux (p lxor n) (n lsr 1)
  in
  aux n (n lsr 1)