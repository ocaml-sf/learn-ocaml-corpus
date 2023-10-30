(* Solution for 1
   let dragon_size n =
     if n >= 0 then (1 lsl n) - 1
     else raise (Invalid_argument "dragon_size")

   (* Solution for 2 *)
   let append l1 l2 =
     let rec aux acc = function
       | [], [] -> List.rev acc
       | [], h :: t -> aux (h :: acc) ([], t)
       | h :: t, l -> aux (h :: acc) (t, l)
     in
     aux [] (l1, l2)

   let rec dragon_aux n t =
     if n <= 1 then t
     else
       let tnew = append t [false] in
       let t = List.fold_left(fun acc e -> (not e)::acc) [] t in
       dragon_aux (n-1) (append tnew t)

   let dragon = function
     | 0 -> []
     | n when n > 0 -> dragon_aux n [false]
     | _ -> raise (Invalid_argument "dragon")

   (* Solution for 3 *)
   let closest_pow2 v =
     let v = v - 1 in
     let v = v lor (v lsr 1) in
     let v = v lor (v lsr 2) in
     let v = v lor (v lsr 4) in
     let v = v lor (v lsr 8) in
     let v = v lor (v lsr 16) in
     v + 1

   let dragon_bit = function
     | n when n > 0 ->
       let v = closest_pow2 n in
       List.nth (dragon v) (n-1)
     | _ -> raise (Invalid_argument "dragon_bit")
*)

(* Another possible solution *)
(* 1 *)
let dragon_size n =
  if n < 0 then raise (Invalid_argument "dragon_size") else (1 lsl n) - 1

(* 2 *)
let rec remove_middle l =
  if List.length l = 1 then []
  else
    let middle = List.length l / 2 in
    let rec loop acc i = function
      | [] -> List.rev acc
      | h :: t ->
          if i = middle then loop acc (i + 1) t else loop (h :: acc) (i + 1) t
    in
    loop [] 0 l

let append l1 l2 =
  let rec loop acc l1 l2 =
    match (l1, l2) with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
  in
  loop [] l1 l2

let add_false l = append l [ false ]
let add_true l = append l [ true ]

let list_copy_index l index_beg index_end =
  let rec loop acc i = function
    | [] -> List.rev acc
    | h :: t ->
        if i >= index_beg && i <= index_end then loop (h :: acc) (i + 1) t
        else loop acc (i + 1) t
  in
  loop [] 0 l

let dragon n =
  if n < 0 then raise (Invalid_argument "dragon")
  else if n = 0 then []
  else
    let rec dragon_aux n acc =
      if n = 1 then acc
      else
        let acc2 = remove_middle acc in
        let tam = List.length acc2 in
        let sub_list1 = list_copy_index acc2 0 ((tam / 2) - 1) in
        let sub_list2 = list_copy_index acc2 (tam / 2) (tam - 1) in
        let acc3 =
          append (add_true (append (add_false acc) sub_list1)) sub_list2
        in
        dragon_aux (n - 1) acc3
    in
    dragon_aux n [ false ]

(* 3 *)
let dragon_bit n : bool =
  if n <= 0 then raise (Invalid_argument "dragon_bit")
  else
    let l = dragon n in
    let x = List.nth l (n - 1) in
    x