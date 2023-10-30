open List

let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let hd_opt = function
  | [] -> None
  | h :: _ -> Some h

let tl_opt = function
  | [] -> None
  | _ :: t -> Some t

let rec list_max = function
  | [] -> None
  | h :: t -> match list_max t with
    | None -> Some h
    | Some m -> Some (max m h)


let rec list_greatest le = function
  | [] -> None
  | h :: t -> Some (match list_greatest le t with
    | None -> h
    | Some m -> if le h m then m else h)


let rec find_opt f = function
  | [] -> None
  | h :: t -> if f h then Some h else find_opt f t

let rec filter_map f = function
  | [] -> []
  | h :: t -> match f h with
    | None -> filter_map f t
    | Some h' -> h' :: filter_map f t

let assoc_opt x l =
  option_map snd (find_opt (fun (k, v) -> k = x) l)


let value default = function
  | None -> default
  | Some x -> x

let average l =
  let lg = map snd l in
  let lc = map (value 0.) lg in
  let sum = fold_left (+.) 0. lc in
  let ll = float_of_int (length lc) in
  if ll = 0.0 then None else Some (sum /. ll)

let average_present l =
  let lg = map snd l in
  let lc = filter_map (fun x -> x) lg in
  let sum = fold_left (+.) 0. lc in
  let ll = float_of_int (length lc) in
  if ll = 0.0 then None else Some (sum /. ll)

let max_grade l =
  l |> map snd |> filter_map (fun x -> x) |> list_max

let best l =
  l |> filter_map (fun (e, g) -> option_map (fun n -> e, n) g)
    |> list_greatest (fun (_, g1) (_, g2) -> g1 < g2)
    |> option_map fst
