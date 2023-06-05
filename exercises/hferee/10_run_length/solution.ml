


let rec encode = function
  | [] -> []
  | h:: t -> match encode t with
    | [] -> (h, 1) :: []
    | (h', n) :: t' ->
      if h = h' then (h, n + 1) :: t'
      else  (h, 1) :: (h', n) :: t'


let rec decode = function
  | [] -> []
  | (h, 0) :: t -> decode t
  | (h, n) :: t -> h :: decode ((h, n - 1) :: t)

let rec split_pairs = function
  | [] -> []
  | (h, h') :: t -> h' :: h :: split_pairs t

let rec mystery n =
  if  n <= 0 then [1]
  else split_pairs (encode (mystery (n - 1)))


(* bonus *)

let list_mystery n=
  let rec aux k last acc =
    if k >= n then last :: acc else
      aux (k + 1) (split_pairs (encode last)) (last :: acc)
  in List.rev (aux 0 [1] [])


