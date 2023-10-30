let hd = function
  | Nothing -> 42
  | OneMore (h, _) -> h

let tl = function
  | Nothing -> Nothing
  | OneMore (_, t) -> t

let rec length = function
  | Nothing -> 0
  | OneMore (_, t) -> 1 + length t

let rec sum_list = function
  | Nothing -> 0
  | OneMore (h, t) -> h + sum_list t

let rec concat l1 l2 = match l1 with
  | Nothing -> l2
  | OneMore (h, t) -> OneMore (h, concat t l2)

let rev =
  let rec rev_aux rl = function
    | Nothing -> rl
    | OneMore (h, t) -> rev_aux (OneMore (h, rl)) t
  in rev_aux Nothing

let rec mem e = function
  | Nothing -> false
  | OneMore (h, t) -> h = e || mem e t


let rec find_first e = function
  | Nothing -> -1
  | OneMore (h, t) ->
    if h = e
      then 0
    else
      let i = find_first e t in
      if i = -1 then -1
      else i + 1


let rec find_last e = function
  | Nothing -> -1
  | OneMore (h, t) ->
      let i = find_last e t in
      if i = -1 then
        if h = e then 0 else -1
      else i + 1

let rec partition p = function
  | Nothing -> Nothing, Nothing
  | OneMore (h, t) ->
    let (l1, l2) = partition p t in
    if p h then OneMore (h, l1), l2
    else l1, OneMore (h, l2)
