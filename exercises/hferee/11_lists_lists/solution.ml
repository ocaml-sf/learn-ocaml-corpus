let rec list_get l n = match l, n with
| [], _ -> None
| h :: t, 0 -> Some h
| h :: t, _ -> list_get t (n - 1)


let list_list_get l n m =
  match list_get l n with
  | None -> None
  | Some l' -> list_get l' m

let hd_opt = function
  | [] -> None
  | h :: _ -> Some h

let tl_opt = function
  | [] -> None
  | _ :: t -> Some t

let rec filter_map f = function
  | [] -> []
  | h :: t -> match f h with
    | None -> filter_map f t
    | Some h' -> h' :: filter_map f t

let rec transpose l = match l with
| [] -> []
| _ -> match filter_map hd_opt l with
  | [] -> []
  | firsts -> firsts :: transpose (filter_map tl_opt l)

let rec merge l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | x1::l1', x2::l2' ->
     if x1 < x2 then
       x1::(merge l1' l2)
     else
       x2::(merge l1 l2')

let wrap a = List.map (fun x -> [x]) a

let rec flatten_merge = function
  | x1::x2::l' ->
     merge x1 x2 :: flatten_merge l'
  | l -> l

let merge_sort l =
  let rec aux l =
    match l with
    | [] -> []
    | xs ::[] -> xs
    | _ -> aux @@ flatten_merge l
  in
  aux (wrap l)
