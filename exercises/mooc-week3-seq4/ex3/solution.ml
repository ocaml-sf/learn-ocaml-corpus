let rec to_list = function
  | CEmpty -> []
  | CSingle x -> [x]
  | CApp (l, r) -> to_list l @ to_list r

let rec of_list = function
  | [] -> CEmpty
  | x :: xs -> CApp (CSingle x, of_list xs)

let append l1 l2 =
  match (l1, l2) with
    | (CEmpty, l)
    | (l, CEmpty) -> l
    | _ -> CApp (l1, l2)

let rec hd = function
  | CEmpty ->
    None
  | CSingle x ->
    Some x
  | CApp (l, r) ->
    match hd l with
      | None -> hd r
      | x -> x

let rec tl = function
  | CEmpty ->
    None
  | CSingle x ->
    Some CEmpty
  | CApp (l, r) ->
    match tl l with
      | Some l' -> Some (append l' r)
      | None -> tl r
