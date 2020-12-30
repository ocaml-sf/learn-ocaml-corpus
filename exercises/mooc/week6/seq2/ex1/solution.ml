module Dict : DictSig = struct
  type ('key, 'value) t =
    | Empty
    | Node of ('key, 'value) t * 'key * 'value * ('key, 'value) t

  let empty = Empty

  let rec add d k v =
    match d with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (l, k', v', r) ->
        if k = k' then Node (l, k, v, r)
        else if k < k' then Node (add l k v, k', v', r)
        else Node (l, k', v', add r k v)

  exception NotFound

  let rec lookup d k =
    match d with
    | Empty ->
	raise NotFound
    | Node (l, k', v', r) ->
	if k = k' then v'
	else if k < k' then lookup l k
	else lookup r k

  let rec remove d k =
    match d with
    | Empty -> Empty
    | Node (l, k', v', r) ->
        if k = k' then
          match l, r with
          | Empty, n | n, Empty -> n
          | l, r ->
              let rec max = function
                | Empty -> assert false
                | Node (_, k, v, Empty) -> (k, v)
                | Node (_, _, _, n) -> max n in
              let mk, mv = max l in
              Node (remove l mk, mk, mv, r)
        else if k < k' then Node (remove l k, k', v', r)
        else Node (l, k', v', remove r k)

end ;;
