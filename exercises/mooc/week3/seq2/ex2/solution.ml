let children_from_char m c =
  let rec aux = function
    | [] -> None
    | (c', t) :: cs -> if c = c' then Some t else aux cs
  in
  aux m

let update_children m c t =
  let rec aux = function
    | [] ->
      [(c, t)]
    | (c', t') :: m ->
      if c = c' then (c, t) :: m else (c', t') :: aux m
  in
  aux m

let lookup trie w =
  let rec aux i (Trie (label, cmap)) =
    if i = String.length w then label else
      match children_from_char cmap w.[i] with
	| None -> None
	| Some t -> aux (i + 1) t
  in
  aux 0 trie

let insert trie w v =
  let rec aux i (Trie (label, cmap)) =
    if i = String.length w then Trie (Some v, cmap) else
      match children_from_char cmap w.[i] with
	| None ->
	  Trie (label, (w.[i], aux (i + 1) (Trie (None, []))) :: cmap)
	| Some t ->
	  Trie (label, update_children cmap w.[i] (aux (i + 1) t))
  in
  aux 0 trie
